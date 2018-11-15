# Red-black tree
Red-black tree is a self balancing binary-search tree. Each node of the tree has an extra parameter (clor). The color paramter helps to ensure that the tree remains balanced after insertions and deletions. The time complexity of search in these type of tree is O(log n) where n is the number of elements in the tree. There are only two colors, hence tracking the color of the node is done in only O(1) time. Traversal is in-order that is left-root-right order. 
## Properties
(1) Each node is either black or red 
(2) The root is black
(3) All leaves (E) are black
(4) Every path from a given node to any of of its descendant nodes contain the same number of black nodes.
(5) If a node is red, then both its children are black.
(6) No red node has a red parent 
(7) There are no two adjacent red nodes.
There is a critical property of red-black tree: The path from the root to the farthest leaf is no more than twice as long as the path from the root to the nearest leaf. This is because the tree is roughly balanced.
