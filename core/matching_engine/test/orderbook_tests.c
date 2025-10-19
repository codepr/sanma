#include "../src/orderbook.h"
#include "test.h"
#include "test_helpers.h"
#include <stdio.h>
#include <stdlib.h>

static inline int run_test(int (*test_func)(Order_Book *book))
{
    Order_Book book = {0};
    orderbook_init(&book);
    int success = test_func(&book);
    orderbook_free(&book);
    return success;
}

static int test_orderbook_add(Order_Book *book)
{
    TEST_HEADER;

    TEST_FOOTER;

    return -1;
}

static int test_orderbook_remove(Order_Book *book)
{
    TEST_HEADER;

    TEST_FOOTER;

    return -1;
}

static int test_orderbook_match(Order_Book *book)
{
    TEST_HEADER;

    TEST_FOOTER;

    return -1;
}

int orderbook_tests(void)
{
    printf("* %s\n\n", __FUNCTION__);

    int cases       = 3;
    int success     = cases;
    Order_Book book = {0};

    orderbook_init(&book);

    success += run_test(test_orderbook_add);
    success += run_test(test_orderbook_remove);
    success += run_test(test_orderbook_match);

    printf("\n Test suite summary: %d passed, %d failed\n", success, cases - success);

    return success < cases ? -1 : 0;
}
