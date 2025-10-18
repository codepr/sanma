#include "iomux.h"
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#define MAX_CONNECTIONS 1024
#define MAX_ORDERS      100000
#define SOCKET_PATH     "/tmp/matching_engine.sock"
#define BUFFER_SIZE     256

// Price range configuration (in cents)
#define MIN_PRICE       0
#define MAX_PRICE       1000000 // $10,000.00
#define PRICE_BUCKETS   (MAX_PRICE - MIN_PRICE + 1)

typedef enum { ORDER_BUY = 0, ORDER_SELL = 1 } Order_Side;
typedef enum { ORDER_LIMIT = 0, ORDER_MARKET = 1 } Order_Type;

// Linked list node for orders at the same price level
typedef struct {
    uint32_t order_idx;
    uint32_t next;
} Order_Node;

typedef struct order {
    uint64_t order_id;
    uint32_t client_id;
    uint32_t price;
    uint32_t quantity;
    uint32_t filled;
    uint16_t side;
    uint16_t type;
} Order;

// Price bucket, cent level precision
typedef struct {
    uint32_t total_quantity;
    uint32_t first_order;
    uint32_t last_order;
} Price_Bucket;

// The main order book struct, tracking BIDs and ASKs. Just a fat struct.
typedef struct {
    // Indexed by price
    Price_Bucket *bid_buckets;
    Price_Bucket *ask_buckets;

    // Orders linked list
    Order_Node *order_nodes;
    uint32_t node_count;
    uint32_t node_capacity;

    // Free list node recycling
    uint32_t free_node_head;

    Order *orders;
    uint32_t order_count;
    uint32_t order_capacity;

    // Free list for order recycling
    uint32_t *free_order_indices;
    uint32_t free_order_count;

    // Best bid/ask tracking
    uint32_t best_bid_price; // Highest bid
    uint32_t best_ask_price; // Lowest ask

    uint64_t next_order_id;
} Order_Book;

// Trade execution result
typedef struct {
    uint64_t buy_order_id;
    uint64_t sell_order_id;
    uint32_t price;
    uint32_t quantity;
    uint32_t buyer_id;
    uint32_t seller_id;
} Trade;

typedef struct {
    int fd;
    uint32_t client_id;
    bool active;
} Connection;

// Engine state
typedef struct {
    Order_Book book;
    Connection connections[MAX_CONNECTIONS];
    int listen_fd;
    uint32_t connection_count;
    uint32_t next_connection_id;
} Matching_Engine;

static void init_orderbook(Order_Book *book)
{
    // Allocate fixed price bucket arrays
    book->bid_buckets = calloc(PRICE_BUCKETS, sizeof(Price_Bucket));
    book->ask_buckets = calloc(PRICE_BUCKETS, sizeof(Price_Bucket));

    // Initialize all buckets as empty
    for (uint32_t i = 0; i < PRICE_BUCKETS; i++) {
        book->bid_buckets[i].first_order = UINT32_MAX;
        book->bid_buckets[i].last_order  = UINT32_MAX;
        book->ask_buckets[i].first_order = UINT32_MAX;
        book->ask_buckets[i].last_order  = UINT32_MAX;
    }

    book->order_nodes        = malloc(MAX_ORDERS * sizeof(Order_Node));
    book->orders             = malloc(MAX_ORDERS * sizeof(Order));
    book->free_order_indices = malloc(MAX_ORDERS * sizeof(uint32_t));

    book->node_count         = 0;
    book->node_capacity      = MAX_ORDERS;
    book->order_count        = 0;
    book->order_capacity     = MAX_ORDERS;
    book->free_order_count   = 0;
    book->free_node_head     = UINT32_MAX;

    book->best_bid_price     = 0;
    book->best_ask_price     = MAX_PRICE;
    book->next_order_id      = 1;
}

static uint32_t alloc_node(Order_Book *book)
{
    if (book->free_node_head != UINT32_MAX) {
        uint32_t node_idx    = book->free_node_head;
        book->free_node_head = book->order_nodes[node_idx].next;
        return node_idx;
    }

    if (book->node_count >= book->node_capacity)
        return UINT32_MAX;

    return book->node_count++;
}

static void free_node(Order_Book *book, uint32_t node_idx)
{
    book->order_nodes[node_idx].next = book->free_node_head;
    book->free_node_head             = node_idx;
}

void update_best_bid(Order_Book *book)
{
    for (int32_t price = book->best_bid_price; price >= MIN_PRICE; price--) {
        if (book->bid_buckets[price].total_quantity > 0) {
            book->best_bid_price = price;
            return;
        }
    }
    book->best_bid_price = 0;
}

void update_best_ask(Order_Book *book)
{
    for (uint32_t price = book->best_ask_price; price <= MAX_PRICE; price++) {
        if (book->ask_buckets[price].total_quantity > 0) {
            book->best_ask_price = price;
            return;
        }
    }
    book->best_ask_price = MAX_PRICE;
}

uint32_t add_order_to_book(Order_Book *book, Order *order)
{
    if (order->price < MIN_PRICE || order->price > MAX_PRICE) {
        return UINT32_MAX;
    }

    // Allocate order slot
    uint32_t order_idx;
    if (book->free_order_count > 0) {
        order_idx               = book->free_order_indices[--book->free_order_count];
        book->orders[order_idx] = *order;
    } else {
        if (book->order_count >= book->order_capacity)
            return UINT32_MAX;
        order_idx               = book->order_count++;
        book->orders[order_idx] = *order;
    }

    // Allocate linked list node
    uint32_t node_idx = alloc_node(book);
    if (node_idx == UINT32_MAX)
        return UINT32_MAX;

    book->order_nodes[node_idx].order_idx = order_idx;
    book->order_nodes[node_idx].next      = UINT32_MAX;

    // Get the appropriate bucket (direct array access - O(1))
    Price_Bucket *bucket = (order->side == ORDER_BUY) ? &book->bid_buckets[order->price]
                                                      : &book->ask_buckets[order->price];

    // Append to bucket's linked list
    if (bucket->first_order == UINT32_MAX) {
        bucket->first_order = node_idx;
        bucket->last_order  = node_idx;
    } else {
        book->order_nodes[bucket->last_order].next = node_idx;
        bucket->last_order                         = node_idx;
    }

    bucket->total_quantity += order->quantity;

    // Update best prices if necessary
    if (order->side == ORDER_BUY && order->price > book->best_bid_price) {
        book->best_bid_price = order->price;
    } else if (order->side == ORDER_SELL && order->price < book->best_ask_price) {
        book->best_ask_price = order->price;
    }

    return order_idx;
}

static void remove_order_from_bucket(Order_Book *book, uint32_t price, Order_Side side,
                                     uint32_t node_idx)
{
    Price_Bucket *bucket =
        (side == ORDER_BUY) ? &book->bid_buckets[price] : &book->ask_buckets[price];

    if (bucket->first_order == node_idx) {
        bucket->first_order = book->order_nodes[node_idx].next;
        if (bucket->last_order == node_idx)
            bucket->last_order = UINT32_MAX;
    } else {
        uint32_t prev = bucket->first_order;
        while (prev != UINT32_MAX && book->order_nodes[prev].next != node_idx)
            prev = book->order_nodes[prev].next;

        if (prev != UINT32_MAX) {
            book->order_nodes[prev].next = book->order_nodes[node_idx].next;
            if (bucket->last_order == node_idx) {
                bucket->last_order = prev;
            }
        }
    }

    free_node(book, node_idx);
}

static void match_orders(Order_Book *book, Trade *trades, uint32_t *trade_count,
                         uint32_t max_trades)
{
    *trade_count = 0;

    while (*trade_count < max_trades) {
        if (book->best_bid_price < book->best_ask_price)
            break;

        if (book->bid_buckets[book->best_bid_price].total_quantity == 0) {
            update_best_bid(book);
            continue;
        }

        if (book->ask_buckets[book->best_ask_price].total_quantity == 0) {
            update_best_ask(book);
            continue;
        }

        Price_Bucket *bid_bucket = &book->bid_buckets[book->best_bid_price];
        Price_Bucket *ask_bucket = &book->ask_buckets[book->best_ask_price];

        if (bid_bucket->first_order == UINT32_MAX || ask_bucket->first_order == UINT32_MAX)
            break;

        uint32_t bid_node_idx  = bid_bucket->first_order;
        uint32_t ask_node_idx  = ask_bucket->first_order;

        Order *bid_order       = &book->orders[book->order_nodes[bid_node_idx].order_idx];
        Order *ask_order       = &book->orders[book->order_nodes[ask_node_idx].order_idx];

        uint32_t bid_remaining = bid_order->quantity - bid_order->filled;
        uint32_t ask_remaining = ask_order->quantity - ask_order->filled;
        uint32_t trade_qty     = (bid_remaining < ask_remaining) ? bid_remaining : ask_remaining;

        Trade *trade           = &trades[(*trade_count)++];
        trade->buy_order_id    = bid_order->order_id;
        trade->sell_order_id   = ask_order->order_id;
        trade->price           = book->best_ask_price;
        trade->quantity        = trade_qty;
        trade->buyer_id        = bid_order->client_id;
        trade->seller_id       = ask_order->client_id;

        bid_order->filled += trade_qty;
        ask_order->filled += trade_qty;
        bid_bucket->total_quantity -= trade_qty;
        ask_bucket->total_quantity -= trade_qty;

        if (bid_order->filled == bid_order->quantity) {
            remove_order_from_bucket(book, book->best_bid_price, ORDER_BUY, bid_node_idx);
            book->free_order_indices[book->free_order_count++] =
                book->order_nodes[bid_node_idx].order_idx;

            if (bid_bucket->total_quantity == 0)
                update_best_bid(book);
        }

        if (ask_order->filled == ask_order->quantity) {
            remove_order_from_bucket(book, book->best_ask_price, ORDER_SELL, ask_node_idx);
            book->free_order_indices[book->free_order_count++] =
                book->order_nodes[ask_node_idx].order_idx;

            if (ask_bucket->total_quantity == 0)
                update_best_ask(book);
        }
    }
}

// NETWORK

// Message types for binary protocol
typedef enum {
    MSG_NEW_ORDER    = 1,
    MSG_ORDER_ACK    = 2,
    MSG_ORDER_REJECT = 3,
    MSG_TRADE        = 4,
    MSG_CANCEL_ORDER = 5,
    MSG_CANCEL_ACK   = 6
} Message_Type;

struct message_header {
    uint8_t type;
    uint8_t padding[3];
    uint32_t length;
};

// New order request (sent by client)
struct order_request_message {
    uint32_t price;
    uint32_t quantity;
    uint16_t side; // ORDER_BUY or ORDER_SELL
    uint16_t type; // ORDER_LIMIT or ORDER_MARKET
};

// Order acknowledgment (sent by engine)
struct order_ack_message {
    uint64_t order_id;
    uint8_t status; // 0 = accepted, 1 = rejected
    uint8_t padding[7];
};

// Trade notification - can be cast directly to Trade struct
struct trade_message {
    uint64_t buy_order_id;
    uint64_t sell_order_id;
    uint32_t price;
    uint32_t quantity;
    uint32_t buyer_id;
    uint32_t seller_id;
};

static int set_nonblocking(int fd)
{
    int flags, result;
    flags = fcntl(fd, F_GETFL, 0);
    if (flags == -1)
        return -1;

    result = fcntl(fd, F_SETFL, flags | O_NONBLOCK);
    if (result == -1)
        return -1;

    return 0;
}

static int unix_socket_listen(const char *sockpath)
{
    // Create Unix socket
    int listen_fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (listen_fd < 0)
        return -1;

    struct sockaddr_un addr;
    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, sockpath, sizeof(addr.sun_path) - 1);

    unlink(sockpath);

    if (bind(listen_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0)
        goto error;

    if (listen(listen_fd, 32) < 0)
        goto error;

    set_nonblocking(listen_fd);

    return listen_fd;

error:
    close(listen_fd);
    return -1;
}

static void process_message(Matching_Engine *engine, uint32_t conn_idx, void *buffer)
{
    uint8_t *packet               = buffer;
    struct message_header *header = (struct message_header *)packet;

    switch (header->type) {
    case MSG_NEW_ORDER:
        if (header->length != sizeof(struct order_request_message))
            return;

        uint8_t *body                               = packet + sizeof(struct message_header);
        struct order_request_message *order_request = (struct order_request_message *)body;

        Order order                                 = {0};
        order.order_id                              = engine->book.next_order_id++;
        order.client_id                             = engine->connections[conn_idx].client_id;
        order.price                                 = order_request->price;
        order.quantity                              = order_request->quantity;
        order.filled                                = 0;
        order.type                                  = ORDER_LIMIT;
        order.side                                  = order_request->side;

        uint32_t result                             = add_order_to_book(&engine->book, &order);

        struct message_header ack_header   = {.type   = (result == UINT32_MAX) ? MSG_ORDER_REJECT
                                                                               : MSG_ORDER_ACK,
                                              .length = sizeof(struct order_ack_message)};
        struct order_ack_message order_ack = {.order_id = order.order_id,
                                              .status   = (result == UINT32_MAX) ? 1 : 0};

        send(engine->connections[conn_idx].fd, &ack_header, sizeof(ack_header), 0);
        send(engine->connections[conn_idx].fd, &order_ack, sizeof(order_ack), 0);

        if (result == UINT32_MAX)
            return;

        // Try to match
        Trade trades[100];
        uint32_t trade_count;
        match_orders(&engine->book, trades, &trade_count, 100);

        if (trade_count > 0) {
            struct message_header trade_header = {.type   = MSG_TRADE,
                                                  .length = sizeof(struct trade_message)};

            for (uint32_t i = 0; i < trade_count; ++i) {
                for (uint32_t c = 0; c < MAX_CONNECTIONS; ++c) {
                    if (engine->connections[c].active) {
                        send(engine->connections[c].fd, &trade_header, sizeof(trade_header), 0);
                        send(engine->connections[c].fd, &trades[i], sizeof(Trade), 0);
                    }
                }
            }
        }
        break;

    default:
        break;
    }
}

static void run_engine(Matching_Engine *engine)
{
    uint8_t buffer[BUFFER_SIZE];
    IO_Mux *iomux = iomux_create();

    iomux_add(iomux, engine->listen_fd, IOMUX_READ);

    for (;;) {
        int numevents = iomux_wait(iomux, -1);
        if (numevents < 0)
            fprintf(stderr, ">>>>: iomux error: %s", strerror(errno));

        for (int i = 0; i < numevents; ++i) {
            int fd = iomux_get_event_fd(iomux, i);

            if (fd == engine->listen_fd) {
                int conn_fd = accept(engine->listen_fd, NULL, NULL);
                if (conn_fd >= 0 && engine->connection_count < MAX_CONNECTIONS) {
                    set_nonblocking(conn_fd);
                    // uint32_t idx                           = engine->connection_count++;
                    engine->connections[conn_fd].fd        = conn_fd;
                    engine->connections[conn_fd].client_id = engine->next_connection_id++;
                    engine->connections[conn_fd].active    = 1;

                    iomux_add(iomux, conn_fd, IOMUX_READ);

                    printf("Client %u connected\n", engine->connections[conn_fd].client_id);
                }
            } else if (engine->connections[fd].fd == fd) {
                // Check client messages
                ssize_t n = recv(fd, buffer, BUFFER_SIZE - 1, 0);
                if (n > 0) {
                    buffer[n] = '\0';
                    process_message(engine, fd, buffer);
                } else {
                    // Client disconnected
                    iomux_del(iomux, engine->connections[fd].fd);
                    close(engine->connections[fd].fd);
                    engine->connections[fd].fd     = -1;
                    engine->connections[fd].active = false;
                    printf("Client %u disconnected\n", engine->connections[fd].client_id);
                }
            }
        }
    }

    iomux_free(iomux);
}

static int init_engine(Matching_Engine *engine)
{
    init_orderbook(&engine->book);

    memset(engine->connections, 0, sizeof(engine->connections));
    engine->connection_count   = 0;
    engine->next_connection_id = 1;

    for (int i = 0; i < MAX_CONNECTIONS; ++i)
        engine->connections[i].fd = -1;

    engine->listen_fd = unix_socket_listen(SOCKET_PATH);
    if (engine->listen_fd < 0)
        return -1;

    return 0;
}

int main(void)
{
    Matching_Engine engine;

    int err = init_engine(&engine);
    if (err < 0) {
        fprintf(stderr, "Error initing matching engine\n");
        exit(1);
    }

    printf("Matching engine listening on %s\n", SOCKET_PATH);
    printf("Price range: $%.2f - $%.2f (1 cent precision)\n", MIN_PRICE / 100.0, MAX_PRICE / 100.0);
    run_engine(&engine);

    return 0;
}
