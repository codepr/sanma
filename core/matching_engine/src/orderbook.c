#include "orderbook.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

// Linked list node for orders at the same price level
struct order_node {
    uint32_t order_idx;
    uint32_t next;
};

// Price bucket, cent level precision
struct price_bucket {
    uint32_t total_quantity;
    uint32_t first_order;
    uint32_t last_order;
};

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

static void update_best_bid(Order_Book *book)
{
    for (int32_t price = book->best_bid_price; price >= MIN_PRICE; price--) {
        if (book->bid_buckets[price].total_quantity > 0) {
            book->best_bid_price = price;
            return;
        }
    }
    book->best_bid_price = 0;
}

static void update_best_ask(Order_Book *book)
{
    for (uint32_t price = book->best_ask_price; price <= MAX_PRICE; price++) {
        if (book->ask_buckets[price].total_quantity > 0) {
            book->best_ask_price = price;
            return;
        }
    }
    book->best_ask_price = MAX_PRICE;
}

void orderbook_init(Order_Book *book)
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

void orderbook_free(Order_Book *book)
{
    assert(book);

    free(book->bid_buckets);
    free(book->ask_buckets);
    free(book->order_nodes);
    free(book->orders);
    free(book->free_order_indices);

    memset(book, 0x00, sizeof(*book));
}

uint32_t orderbook_add(Order_Book *book, const Order *order)
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

void orderbook_remove(Order_Book *book, uint32_t price, Order_Side side, uint32_t node_idx)
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

void orderbook_match(Order_Book *book, Trade *trades, uint32_t *trade_count, uint32_t max_trades)
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
            orderbook_remove(book, book->best_bid_price, ORDER_BUY, bid_node_idx);
            book->free_order_indices[book->free_order_count++] =
                book->order_nodes[bid_node_idx].order_idx;

            if (bid_bucket->total_quantity == 0)
                update_best_bid(book);
        }

        if (ask_order->filled == ask_order->quantity) {
            orderbook_remove(book, book->best_ask_price, ORDER_SELL, ask_node_idx);
            book->free_order_indices[book->free_order_count++] =
                book->order_nodes[ask_node_idx].order_idx;

            if (ask_bucket->total_quantity == 0)
                update_best_ask(book);
        }
    }
}
