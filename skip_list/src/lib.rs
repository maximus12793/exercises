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

impl<T> SkipList<T>
where
    T: Eq + Ord + Default + Copy + Clone,
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
        // Determine the height of the new node
        let mut height = 0;
        // The rand::random() is used to randomly distribute nodes at various
        // heights in the skip list which improves performance.
        while height < self.height && rand::random() {
            height += 1;
        }

        // Create the new node and insert it into the list
        let mut new_node = Box::new(Node {
            element,
            next: vec![None; height + 1],
            prev: vec![None; height + 1],
        });

         // Find the position where the new node should be inserted
         let mut curr = &mut self.head;
         let mut update = vec![self.head.clone(); self.height];
         for i in (0..self.height).rev() {
             while let Some(ref next) = curr.next[i] {
                 if next.element > element {
                     break;
                 }
                 curr = &mut *next;
             }
             update[i] = curr.clone();
         }

        // Insert the new node
        for i in 0..=height {
            new_node.next[i] = update[i].next[i].take();
            update[i].next[i] = Some(new_node.clone());
            if let Some(ref mut next) = new_node.next[i] {
                next.prev[i] = Some(new_node.clone());
            }
        }

        // Update the tail if necessary
        if let Some(ref mut tail) = self.tail.prev[0] {
            if tail.element < element {
                self.tail = new_node;
            }
        }

        // Update the size and height of the list
        self.size += 1;
        self.height = self.height.max(height + 1);
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