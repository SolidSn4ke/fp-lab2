module Node (
    Node (..), insertNode
) where

data Node a = Null | Node Int a Int (Node a) deriving (Show, Eq)

insertNode :: Node a -> Node a -> Node a
insertNode Null newNode = newNode
insertNode newNode Null = newNode
insertNode (Node h k v next) newNode = if h == h'
    then Node h k (v + 1) next
    else case next of
        Null -> Node h k v newNode
        _    -> if h == h'
            then Node h k (v + 1) next
            else Node h k v (insertNode next newNode)
    where
        Node h' _ _ _ = newNode