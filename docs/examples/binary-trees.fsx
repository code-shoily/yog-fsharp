(**
# Binary Trees
A binary tree is a tree data structure in which each node has at most two children, which are referred to as the left child and the right child.

*)

open Yog.Model
open Yog.Generators

let tree = Classic.binaryTree 2 Undirected

(**
## Visual Representation

<div class="mermaid">
graph TD
  0 --- 1
  0 --- 2
  1 --- 3
  1 --- 4
  2 --- 5
  2 --- 6
</div>
*)
