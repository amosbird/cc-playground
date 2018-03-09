// === C++ Playground ===
// Execute the snippet with Ctrl-Return
// Remove the snippet completely with its dir and all files M-x `cc-playground-rm`

#include <gtest/gtest.h>
using namespace std;

class SnippetTest : public testing::Test {
protected:
    // Adding fixtures
    SnippetTest() {
        // setup
    }

    virtual ~SnippetTest() {
        // teardown
    }

    virtual void SetUp() {
        // use this if fixture class is needed without gtest.
    }

    virtual void TearDown() {
        // use this if fixture class is needed without gtest.
    }
};

TEST_F(SnippetTest, foo) {
    // simple test
    ASSERT_EQ(10, 10);
}
