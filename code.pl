% Roshan Prashant Bara
% ENTRY NO.: 2020CS10377


/* Integer Binary Tree Definition */
ibt(empty).
ibt(node(N, L, R)):- integer(N), ibt(L), ibt(R).

/* Integer Binary Tree Size */
size(empty, 0).
size(node(X, L, R), N) :- ibt(node(X, L, R)), size(L,S1), size(R,S2), N is 1+S1+S2.
% Calculates max of 2 Integers
max(X,Y,X) :- X >= Y,!.
max(X,Y,Y) :- X < Y.

/* Integer Binary Tree Height assuming height of empty tree is -1*/
height(empty,-1).
height(node(X, L, R), N) :- ibt(node(X, L, R)), height(L, LH), height(R, RH),
                    max(LH,RH,Y), N is 1+Y.


/* Integer Binary Tree Preorder Traversal returns an empty list if BT is empty*/
preorder(empty,[]).
preorder(node(X, LC, RC),[X|Rest]) :- ibt(node(X, LC, RC)), preorder(LC, L1), preorder(RC, L2),
                    append(L1,L2,Rest).

/* Helper to transform an element to a list */
transform(X,L,[X|L]).

/* Integer Binary Tree Inorder Traversal returns an empty list if BT is empty*/
inorder(empty,[]).
inorder(node(N, LC, RC),L) :- ibt(node(N, LC, RC)), inorder(LC, L1), inorder(RC, L2),
                    transform(N,L2,L3), append(L1,L3,L).

/* Integer Binary Tree postorder Traversal returns an empty list if BT is empty*/
postorder(empty,[]).
postorder(node(N, LC, RC),L) :- ibt(node(N, LC, RC)), postorder(LC, L1), postorder(RC, L2),
                    transform(N,[],L3), append(L1,L2,L4), append(L4,L3,L).

% Push for Stack
push(L, empty, L):-!.
push(L, X, [X|L]).

% Tail-recursive code for Preorder Traversal
trPreorder(BT, L) :- ibt(BT), append([BT], [], Stack), gentrPreorder(Stack, [], L).
gentrPreorder([], L, L).
gentrPreorder([H|T], Lsofar, NewL) :- H = node(X,L,R),
                    push(T,R, Stack2), push(Stack2, L, Stack3), append(Lsofar, [X], L1), gentrPreorder(Stack3, L1, NewL).

% Tail-recursive code for Inorder Traversal
trInorder(BT, L1) :- ibt(BT), gentrInorder([], BT, [], L1).
gentrInorder([], empty, L, L):-!.
gentrInorder([H|T], empty, Lsofar, NewL) :- H = node(X,_,R), append(Lsofar, [X], L1), gentrInorder(T, R, L1, NewL),!.
gentrInorder(Stack, node(X,L,R), Lsofar, NewL) :- append([node(X,L,R)], Stack, Stack2), gentrInorder(Stack2, L, Lsofar, NewL).

% Tail-recursive code for Postorder Traversal
trPostorder(BT, L1) :- ibt(BT), gentrPostorder([], [], BT, [], L1).
gentrPostorder([], _, empty, L, L):-!.
gentrPostorder([H|T], [A|B], empty, Lsofar, NewL) :- H = node(X,_,R), append([H], T, Stack1), (R == A -> gentrPostorder(Stack1, B, A, Lsofar, NewL); (append(Lsofar, [X], L1), gentrPostorder(T, [A|B], empty, L1, NewL))),!. 
gentrPostorder([H|T], [], empty, Lsofar, NewL) :- H = node(X,_,_), append(Lsofar, [X], L1), gentrPostorder(T, [], empty, L1, NewL),!.
gentrPostorder(Stack, TmpStack, node(X,L,R), Lsofar, NewL) :- push(TmpStack, R, TmpStack2), append([node(X,L,R)], Stack, Stack2), gentrPostorder(Stack2, TmpStack2, L, Lsofar, NewL).




% generates Euler Tour from BT
eulerTour(BT, L) :- ibt(BT),eulerTourGen(BT, [], L).
eulerTourGen(empty, L, L):-!.
eulerTourGen(node(X, LC, RC), L, FL) :- ibt(node(X, LC, RC)), transform(X, [], L1), append(L, L1, L2), eulerTourGen(LC, L2, L3),
                    append(L3, L1, L4), eulerTourGen(RC, L4, L5), append(L5, L1, FL).

% Helper to find number of occurences of an element in a list 
listSearch(_, [], 0).
listSearch(X, [Head|Tail], Res) :- listSearch(X, Tail, Y), (X == Head -> Res is Y+1 ; Res is Y).

% Generates preorder traversal from Euler Tour
preET(BT, L) :- ibt(BT), eulerTour(BT, L1), preETGen(L1, [], L).
preETGen([], L, L).
preETGen([X|Tail], L2, FL) :- ((listSearch(X, Tail, Y), Z is Y mod 3, Z == 2) -> (transform(X,[],L1), append(L2, L1, L3), preETGen(Tail, L3, FL)); preETGen(Tail, L2, FL)).

% Reverses List
listReverse([],[]).
listReverse([H|T],L) :- transform(H,[],L1), listReverse(T,L2), append(L2, L1, L).

% Generates inorder traversal from Euler Tour
inET(BT, L) :- ibt(BT), eulerTour(BT, L1), inETGen(L1, [], L).
inETGen([], L, L).
inETGen([X|Tail], L2, FL) :- ((listSearch(X, Tail, Y), Z is Y mod 3, Z == 1) -> (transform(X,[],L1), append(L2, L1, L3), inETGen(Tail, L3, FL)); inETGen(Tail, L2, FL)).

% Generates postorder traversal from Euler Tour
postET(BT, L) :- ibt(BT), eulerTour(BT, L1), postETGen(L1, [], L).
postETGen([], L, L).
postETGen([X|Tail], L2, FL) :- ((listSearch(X, Tail, Y), Z is Y mod 3, Z == 0) -> (transform(X,[],L1), append(L2, L1, L3), postETGen(Tail, L3, FL)); postETGen(Tail, L2, FL)).


% Stores the BT as String og=f form “(N, LBT, RBT)”
toString(BT, S) :- ibt(BT), toStringGen(BT, X), atom_string(X, S).
toStringGen(empty, X) :- string_to_atom("()",X).
toStringGen(node(X,LC,RC), S) :- string_to_atom("(",S1), string_to_atom(", ",S2), string_to_atom(")",S3), number_string(X,S10), string_to_atom(S10,S4),
                            toString(LC, S20),toString(RC, S30), atom_concat(S1,S4,S5),atom_concat(S5,S2,S6),atom_concat(S6,S20,S7),atom_concat(S7,S2,S8),
                            atom_concat(S8,S30,S9), atom_concat(S9,S3,S).

% Calculates absolute difference
absolutediff(X, Y, R) :- (X>Y -> (R is X-Y); (R is Y-X)).

% Checks whether BT is balanced
isBalanced(empty).
isBalanced(node(N, L, R)) :- ibt(node(N, L, R)), height(L, H1), height(R, H2), absolutediff(H1, H2, X), 1>=X.



% Integer Binary Tree Definition
isBST(empty).
isBST(node(_,empty,empty)):-!.
isBST(node(N,node(X,L,R),empty)) :- integer(N), X<N, isBST(node(X,L,R)),!.
isBST(node(N,empty,node(X,L,R))) :- integer(N), N<X, isBST(node(X,L,R)),!.
isBST(node(N,node(X1,L1,R1),node(X2,L2,R2))) :- integer(N), X1<N, N<X2, isBST(node(X1,L1,R1)), isBST(node(X2,L2,R2)).

% Generates a balanced BST from list L using helper Function generateBST
makeBST(L, BST) :- sort(L, SL), length(SL, N), generateBST(SL, 1, N, BST).

% Helper generateBST for makeBST
generateBST(SL, LP, LP, BST) :- nth1(LP, SL, X, _), BST = node(X, empty, empty),!.
generateBST(SL, LP, RP, BST) :- (RP<LP -> BST = empty; RP>LP -> generatealterBST(SL, LP, RP, BST)),!.
generatealterBST(SL, LP, RP, BST) :- MID is (LP+RP)//2, A is MID-1, B is MID+1, nth1(MID, SL, X, _), generateBST(SL, LP, A, LBST),
                                generateBST(SL, B, RP, RBST), BST = node(X, LBST, RBST).


% True if there is a node labelled N in BST
lookup(N, node(X,L,R)) :- isBST(node(X,L,R)), (N = X -> N = X; (N < X) -> lookup(N, L); (N > X) -> lookup(N, R)).


% Inserts N in BST1 and stores it in BST2 else if N is already present in BST1 then states false
insert(N, BST1, BST2) :- isBST(BST1), insertfunc(N, BST1, BST2).
% Helper insertfunc
insertfunc(N, empty, node(N, empty, empty)):-!.
insertfunc(N, node(X,L,R), node(X, LC, R)) :- N<X, insertfunc(N,L,LC),!.
insertfunc(N, node(X,L,R), node(X,L,RC)) :- N>X, insertfunc(N,R,RC).


% Inorder Successor for delete function to replace the deleted node
inordersuccessor(node(X,empty,R),X,R).
inordersuccessor(node(X,L,R),Z,node(X,LC,R)):- inordersuccessor(L,Z,LC).

% delete for BST which returns BST2 if label is present else returns false.
delete(X,node(X,L,empty),L):-!.
delete(X,node(X,L,node(R,empty,RR)),node(R,L,RR)):-!.
delete(X,node(X,L,node(R,RL,RR)),node(Y,L,node(R,Z,RR))):- inordersuccessor(RL,Y,Z),!.
delete(N,node(X,L,R),node(X,LC,RC)):- (N<X -> (RC = R, delete(N,L,LC)); N>X -> (LC = L, delete(N,R,RC))).
