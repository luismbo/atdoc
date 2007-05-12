;;; -*- mode: lisp; indent-tabs: nil; show-trailing-whitespace: t -*-

(in-package :cl-user)

(defpackage :blocks-world
  (:use :cl)
  (:export #:basic-block
	   #:block-name
	   #:block-width
	   #:block-height
	   #:block-position
	   #:block-supported-by

	   #:movable-block

	   #:load-bearing-block
	   #:block-support-for

	   #:brick
	   #:wedge
	   #:ball
	   #:table

	   #:hand
	   #:hand-name
	   #:hand-position
	   #:hand-grasping

	   #:put-on
	   #:get-space
	   #:grasp
	   #:ungrasp
	   #:get-rid-of
	   #:make-space
	   #:clear-top
	   #:remove-support
	   #:add-support
	   #:move)
  (:documentation
   "This package contains the source code of chapter 21, @em{\"The Blocks
    World with Classes and Methods\"} from
    @a[http://www.amazon.com/Lisp-3rd-Patrick-Winston/dp/0201083191]{
      Lisp (3rd edition)}
    by Winston and Horn.
    @begin[A picture of the world]{section}
    The block objects represent a world that \"looks\" like this:
    @begin{pre}
/----\\    ^    /---------\\      ^
| b4 |   /  \\  |         |     / \\
\\____/  /_w7_\\ |         |     / \\
/----\\  /----\\ |         |    /   \\  /--------\\        /^\\
| b1 |  | b2 | | b3      |    /   \\  | b6     |       (l8 )
\\____/  \\____/ \\_________/   /_w5__\\ \\________/        \\./
+-----------------------------------------------------------+
|                                                           |
+-----------------------------------------------------------+
    @end{pre}
    @end{section}
    @begin[Example]{section}
    In the initial configuration, where all blocks have been placed directly
    on the table (not shown), @fun{put-on} will move the objects like this:
    @begin{pre}
BLOCKS-WORLD> (put-on b1 b2)
Move hand to pick up B1 at location (1 2).
Grasp B1.
Removing support relations between B1 and TABLE.
Move B1 to top of B2 at location (2 2).
Adding support relations between B1 and B2.
Ungrasp B1.
T
    @end{pre}
    @end{section}
    @begin[The different kinds of blocks]{section}
    Movable blocks than can be moved onto load supporting blocks.  Using
    multiple inheritance, there are also blocks that can do both.

    @aboutclass{basic-block}
    @aboutclass{load-bearing-block}
    @aboutclass{movable-block}
    @end{section}
    @begin[Block properties]{section}
    Slot readers:

    @aboutfun{block-name}
    @aboutfun{block-position}
    @aboutfun{block-width}
    @aboutfun{block-height}
    @aboutfun{block-supported-by}
    @aboutfun{block-support-for}
    @end{section}
    @begin[Concrete block classes]{section}
    These are the blocks found in our world:

    @aboutclass{table}
    @aboutclass{brick}
    @aboutclass{wedge}
    @aboutclass{ball}
    @end{section}
    @begin[The hand]{section}
    The hand is movable.  It can hold at most one block.

    @aboutclass{hand}
    @aboutfun{hand-name}
    @aboutfun{hand-position}
    @aboutfun{hand-grasping}
    @end{section}"))

(defpackage :blocks-world-goals
  (:use :cl :blocks-world)
  (:export #:node
	   #:node-parent
	   #:node-children
	   #:node-action
	   #:*current-node*
	   #:attach-parent
	   #:attach-action
	   #:define-history-method
	   #:show-simple-tree
	   #:find-action
	   #:tell-why)
  (:documentation
   "This package contains the source code of chapter 22, @em{\"Answering
    Questions about Goals\"} from
    @a[http://www.amazon.com/Lisp-3rd-Patrick-Winston/dp/0201083191]{
      Lisp (3rd edition)}
    by Winston and Horn.

    @begin[Lots of undocumented functions]{section}
    I was too lazy to document this package, which is why all its functions
    have a big fat \"undocumented\" warning.

    This package's page also looks rather empty and sad.
    @end{section}"))
