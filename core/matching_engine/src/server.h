#pragma once

#include "orderbook.h"
#include <signal.h>
#include <stdbool.h>
#include <stdint.h>

#define MAX_CONNECTIONS 1024
#define SOCKET_PATH     "/tmp/matching_engine.sock"

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

typedef struct {
    int fd;
    uint32_t client_id;
    bool active;
} Connection;

// Engine server state
typedef struct engine_server {
    Order_Book book;
    Connection connections[MAX_CONNECTIONS];
    int listen_fd;
    /*
     * This volatile variable is used to signal the main loop to exit.
     * It is declared as volatile to prevent the compiler from optimizing it out.
     * It is also sig_atomic_t to ensure that reads and writes are atomic
     * and won't be interrupted by the signal handler.
     */
    volatile sig_atomic_t is_running;
    uint32_t connection_count;
    uint32_t next_connection_id;
} Engine_Server;

int engine_server_start(Engine_Server *server);
void engine_server_stop(Engine_Server *server);
