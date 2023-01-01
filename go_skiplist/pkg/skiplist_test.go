package skiplist_lib

import (
	"fmt"
	"testing"
)

func TestSkipList(t *testing.T) {
	list := NewSkipList()

	// Insert some key-value pairs into the skip list.
	list.Insert(1, "a")
	list.Insert(2, "b")
	list.Insert(3, "c")
	list.Insert(4, "d")
	list.Insert(5, "e")

	// Check that the values can be retrieved using the Find method.
	for i := 1; i <= 5; i++ {
		expected := fmt.Sprintf("%c", rune('a'+i-1))
		actual := list.Find(i)
		if actual != expected {
			t.Errorf("unexpected value for key %d: expected %s, got %s", i, expected, actual)
		}
	}
}
