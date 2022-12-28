// Note: We are using <Box> for heap allocations out of the box.

// Bidirectional Node elem.
#[derive(Clone)]
struct Node<T>
where
    T: Eq + Ord + Default + Copy,
{
    element: T,
    next: Vec<Option<Box<Node<T>>>>,
    prev: Vec<Option<Box<Node<T>>>>,
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
}

impl<T: Eq + Ord + Default + Copy> SkipList<T>
{
    // Init method.
    fn new() -> Self {
        let default_element = T::default();
        SkipList {
            head: Box::new(Node { element: default_element, next: vec![], prev: vec![] }),
            tail: Box::new(Node { element: default_element, next: vec![], prev: vec![] }),
            height: 1,
            size: 0,
            max_size: 10,
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
        let mut new_node = Node {
            element,
            next: vec![],
            prev: vec![],
        };
        let mut current_node = &mut self.head;
        let mut level = self.height - 1;
        while level >= 0 {
            while let Some(ref mut next) = current_node.next[level] {
                // Keep cycling until position hit.
                if next.element < element {
                    current_node = &mut next;
                } else {
                    break;
                }
            }
            // Update the next and prev pointers to new node.
            new_node.prev[level] = Some(current_node.clone());
            new_node.next[level] = current_node.next[level].clone();
            current_node.next[level] = Some(Box::new(new_node.clone()));
            // Always decrease level.
            level -= 1;
        }
        self.size += 1;
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