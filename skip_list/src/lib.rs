// Note: We are using <Box> for heap allocations out of the box.
extern crate rand;

// Bidirectional Node elem.
#[derive(Clone)]
struct Node<T>
where
    T: Eq + Ord + Default + Copy,
{
    element: T,
    next: Vec<Option<Box<Node<T>>>>,
    prev: Vec<Option<Box<Node<T>>>>,
    index: usize,
}

struct SkipList<T> 
where 
    T: Eq + Ord + Default + Copy,
{
  head: Box<Node<T>>,
  tail: Box<Node<T>>,
  height: usize, 
  size : usize,
  max_size: usize,
  nodes: Vec<Node<T>>,
}

impl<T> SkipList<T>
where
    T: Eq + Ord + Default + Copy + Clone,
{
    // Init method.
    fn new() -> Self {
        let default_element = T::default();
        SkipList {
            head: Box::new(Node { element: default_element, next: vec![], prev: vec![], index: 0}),
            tail: Box::new(Node { element: default_element, next: vec![], prev: vec![], index: 1}),
            height: 1,
            size: 0,
            max_size: 10,
            nodes: Vec::new(),
        }
    }

    // Search helper method.
    fn search(&self, element: T) -> Option<&Node<T>>{
        let mut current_node = &self.head;
        for i in (0..self.height).rev() {
            while let Some(node) = &current_node.next[i] {
                if node.element >= element {
                    if node.element == element {
                        return Some(node);
                    }
                    break;
                }
                current_node = node;
            }
        }
        None
    }

    // Insert helper method.
    fn insert(&mut self, element: T) {
        // Determine the height of the new node
        let mut height = 0;
        // The rand::random() is used to randomly distribute nodes at various
        // heights in the skip list which improves performance.
        while height < self.height && rand::random() {
            height += 1;
        }

        // Create the new node and insert it into the list
        let new_node = Node {
            element,
            next: vec![None; height + 1],
            prev: vec![None; height + 1],
            index: self.nodes.len(),
        };
        self.nodes.push(new_node);
        let index = self.nodes.len() - 1;
        let new_node = &mut self.nodes[index];

        // Find the position where the new node should be inserted
        let mut curr = 0; // Change the type of `curr` to an `usize`
        let mut update = vec![0; self.height];
        for i in (0..self.height).rev() {
            while let Some(next) = self.nodes[curr].next[i] { // Use indexing instead of dereferencing
                if self.nodes[next.index].element > element { // Use indexing instead of dereferencing
                    break;
                }
                curr = next.index; // Update `curr` using an index rather than a reference
            }
            update[i] = curr;
        }
      
    }
    
}



#[test]
fn test_insert_and_search() {
    let mut list = SkipList::new();

    // Insert some elements into the skip list
    list.insert(1);
    list.insert(2);
    list.insert(3);
    list.insert(4);
    list.insert(5);

    // Check that the elements can be found in the skip list
    assert!(list.search(1).is_some());
    assert!(list.search(2).is_some());
    assert!(list.search(3).is_some());
    assert!(list.search(4).is_some());
    assert!(list.search(5).is_some());

    // Check that non-existent elements are not found in the skip list
    assert!(list.search(6).is_none());
    assert!(list.search(7).is_none());
}


/*
TBD tomorrow:::

#[test]
fn test_search_and_delete() {
    let mut list = SkipList::new();

    // Insert some elements into the skip list
    list.insert(1);
    list.insert(2);
    list.insert(3);
    list.insert(4);
    list.insert(5);

    // Search for and delete the element 3
    assert!(list.search(3).is_some());
    list.delete(3);
    assert!(list.search(3).is_none());

    // Search for and delete the element 1
    assert!(list.search(1).is_some());
    list.delete(1);
    assert!(list.search(1).is_none());

    // Search for and delete the element 5
    assert!(list.search(5).is_some());
    list.delete(5);
    assert!(list.search(5).is_none());

    // Check that the remaining elements can still be found
    assert!(list.search(2).is_some());
    assert!(list.search(4).is_some());
}
*/