#pragma once

#include <stdint.h>

#define MAX_ORDERS    100000

// Price range configuration (in cents)
#define MIN_PRICE     0
#define MAX_PRICE     1000000 // $10,000.00
#define PRICE_BUCKETS (MAX_PRICE - MIN_PRICE + 1)

typedef enum { ORDER_BUY = 0, ORDER_SELL = 1 } Order_Side;
typedef enum { ORDER_LIMIT = 0, ORDER_MARKET = 1 } Order_Type;

typedef struct order {
    uint64_t order_id;
    uint32_t client_id;
    uint32_t price;
    uint32_t quantity;
    uint32_t filled;
    uint16_t side;
    uint16_t type;
} Order;

typedef struct order_node Order_Node;
typedef struct price_bucket Price_Bucket;
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

void orderbook_init(Order_Book *book);
void orderbook_free(Order_Book *book);
uint32_t orderbook_add(Order_Book *book, const Order *order);
void orderbook_remove(Order_Book *book, uint32_t price, Order_Side side, uint32_t node_idx);
void orderbook_match(Order_Book *book, Trade *trades, uint32_t *trade_count, uint32_t max_trades);
