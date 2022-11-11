open System
let DFS (g: list<list<int>>) start =
  // a stack with a beginning value of the starting node
  let stack = [start]

  // list that keeps track of all visited nodes
  let flags = List.map (fun x -> false) g

  // tail represents the output, a string containing the search
  let rec aux (g: list<list<int>>) (flags:list<bool>) tail stack =
    
    if List.isEmpty stack then
      tail
    else
      // peekingthe top of the stack
      let curr = List.head stack

      // setting the current node as visited
      let nFlags = List.mapi (fun i x -> x || (i = curr)) flags

      // adding the current node's unvisited neighbours at the top of the stack
      // also filtering every visited node from the stack
      (List.filter (fun x -> not nFlags.[x]) g.[curr])@
      (List.filter (fun x -> not nFlags.[x]) stack)
      |> aux g nFlags (tail + " " + curr.ToString())
      
  aux g flags "" stack
    
let BFS (g: list<list<int>>) start =
  let queue = [start]
  let flags = List.map (fun x -> false) g


  let rec aux (g: list<list<int>>) (flags:list<bool>) tail queue =
    
    if List.isEmpty queue then
      tail
    else
      // peekingthe front of the queue
      let curr = List.head queue

      // setting the current node as visited
      let nFlags = List.mapi (fun i x -> x || (i = curr)) flags

      // adding the current node's unvisited neighbours at the end of the queue
      // also filtering every visited node from the queue
      (List.filter (fun x -> not nFlags.[x]) queue)@
      (List.filter (fun x -> not nFlags.[x]) g.[curr])
      |> aux g nFlags (tail + " " + curr.ToString())
      
  aux g flags "" queue
    
  


[<EntryPoint>]
let main argv =
    let n0 = [ 2; 4; ]
    let n1 = [ 3; ]
    let n2 = [ 1; 5 ]
    let n3 = [ 0; 4 ]
    let n4 = [ 1; 2; 3; ]
    let n5 = [ 0 ]
    let g = [ n0; n1; n2; n3; n4; n5 ]
    printfn "%s" (DFS g 1)
    printfn "%s" (BFS g 1)
    0 // return an integer exit code
