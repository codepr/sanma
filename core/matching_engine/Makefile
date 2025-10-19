CC=clang
CFLAGS=-Wall                                  \
       -Werror                                \
       -pedantic                              \
       -ggdb                                  \
       -std=c2x                               \
       -fsanitize=address                     \
       -fsanitize=undefined                   \
       -fno-omit-frame-pointer                \
       -Wno-gnu-zero-variadic-macro-arguments \
	   -I/opt/homebrew/include

LDFLAGS = -L/opt/homebrew/lib -lprofiler

ME_SRC = src/main.c       \
         src/iomux.c      \
	 src/orderbook.c  \
	 src/server.c     \
	 src/timeutil.c

ME_OBJ = $(ME_SRC:.c=.o)
ME_EXEC = matching-engine-dev

TEST_SRC = src/orderbook.c        \
	   test/test.c            \
           test/orderbook_tests.c

TEST_OBJ = $(TEST_SRC:.c=.o)
TEST_EXEC = matching-engine-tests

# Release Build Variables
CFLAGS_RELEASE = -Wall -pedantic -std=c2x -O3
ME_EXEC_RELEASE = matching-engine

all: $(ME_EXEC) $(TEST_EXEC)

release: $(ME_EXEC_RELEASE)

$(ME_EXEC): $(ME_OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(ME_EXEC_RELEASE): $(ME_SRC:.c=.o.release)
	$(CC) $(CFLAGS_RELEASE) -o $@ $^ $(LDFLAGS)

%.o.release: %.c
	$(CC) $(CFLAGS_RELEASE) -c $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

$(TEST_EXEC): $(TEST_OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	rm -f $(ME_OBJ) $(ME_EXEC) $(ME_EXEC_RELEASE) $(ME_SRC:.c=.o.release)
	rm -f $(TEST_OBJ) $(TEST_EXEC)

.PHONY: all clean

