Looking at [prob.f90](https://people.sc.fsu.edu/~jburkardt/f_src/prob/prob.f90), one of Burkardt's codes that I am trying to incorporate in my program, and his other codes, I would like to 

(1) change declarations such as 
`real ( kind = 8 ) a` 
to 
`real(kind=dp) :: a` with `dp` defined at the module level 

(2) declare subroutines and functions to be PURE (most of the procedures he defines are PURE but are not declared as such)

(3) specify INTENTs of all arguments -- required for (2)

(4) put code in modules and put IMPLICIT NONE at the top of the module rather than in each procedure

(5) Declare parameters at the module level rather than in procedures. For example the statement 
`real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00`
appears many times in prob.f90

(6) Replace bare end statements at the ends of procedures with `end function foo` or `end subroutine foo`

(7) Find procedures that are defined many times across source files and collect them in a  utility module. For example there is a subroutine timestamp that is defined in hundreds of his source files.

(8) Remove lines with only ! and spaces

(9) Put licensing and author information each on one line at the top. Put modification date on one line in each procedure
