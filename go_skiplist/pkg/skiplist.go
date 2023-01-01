package skiplist_lib

import "math/rand"

const maxLevel = 32

// Node is a node in the skip list.
type Node struct {
	key   int
	value interface{}
	next  []*Node
}

// SkipList is a skip list.
type SkipList struct {
	head  *Node
	level int
}

// NewSkipList creates a new skip list.
func NewSkipList() *SkipList {
	return &SkipList{
		head: &Node{
			next: make([]*Node, maxLevel),
		},
		level: 1,
	}
}

// Get a random level from the skip list.
func (s *SkipList) randomLevel() int {
	level := 1
	for float64(rand.Intn(100)) < 50 {
		level++
	}
	if level > maxLevel {
		level = maxLevel
	}
	return level
}

// Insert inserts a key-value pair into the skip list.
func (s *SkipList) Insert(key int, value interface{}) {
	current := s.head
	update := make([]*Node, maxLevel)
	for i := s.level - 1; i >= 0; i-- {
		for current.next[i] != nil && current.next[i].key < key {
			current = current.next[i]
		}
		update[i] = current
	}
	current = current.next[0]
	if current == nil || current.key != key {
		level := s.randomLevel()
		if level > s.level {
			for i := s.level; i < level; i++ {
				update[i] = s.head
			}
			s.level = level
		}
		node := &Node{
			key:   key,
			value: value,
			next:  make([]*Node, level),
		}
		for i := 0; i < level; i++ {
			node.next[i] = update[i].next[i]
			update[i].next[i] = node
		}
	} else {
		current.value = value
	}
}

// Find finds a key in the skip list and returns its value. If the key is not
// found, it returns nil.
func (s *SkipList) Find(key int) interface{} {
	current := s.head
	for i := s.level - 1; i >= 0; i-- {
		for current.next[i] != nil && current.next[i].key < key {
			current = current.next[i]
		}
	}
	current = current.next[0]
	if current != nil && current.key == key {
		return current.value
	}
	return nil
}
