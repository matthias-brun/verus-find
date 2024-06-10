#[derive(Debug)]
pub struct RangeTree(Vec<RangeTreeNode>);

#[derive(Debug)]
struct RangeTreeNode {
    start: usize,
    end: usize,
    children: RangeTree,
}

impl RangeTree {
    pub fn new() -> Self {
        RangeTree(vec![])
    }

    pub fn insert(&mut self, (start, end): (usize, usize)) -> Result<(),()> {
        if let Some(idx) = self.0.iter().position(|e| start >= e.start) {
            let e = &self.0[idx];
            if start >= e.end {
                // insert a new node
                if self.0.len() > idx + 1 && self.0[idx + 1].start < end {
                    return Err(()); // not nested
                }
                self.0.insert(idx + 1, RangeTreeNode {
                    start, end, children: RangeTree(vec![]),
                });
                Ok(())
            } else {
                if end > self.0[idx].end {
                    return Err(()); // not nested
                }
                // it's a child of this node
                self.0[idx].children.insert((start, end))
            }
        } else {
            self.0.push(RangeTreeNode {
                start, end, children: RangeTree(vec![]),
            });
            Ok(())
        }
    }
}
