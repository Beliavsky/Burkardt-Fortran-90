!  Here are some floating comments.
!
  write ( *, * ) ' '
  write ( *, * ) 'This is a main program with no PROGRAM statement.'
  stop
end
real ( kind = 8 ) function mu ( x )
!*****************************************************************************80
!
!! mu() is a real function with a "real ( kind = 8 )" type statement.
!
!  Currently, the program doesn't understand what is going on here,
!  and redirects this text to the "no_name.f90" file.
!
  real x
  mu = 1.0 / x
  return
end
function beta ( y, delta, gamma )
!*****************************************************************************80
!
!! beta() is a function, but this is the second module with this name!
!
  beta = 17.0
  return
end
block data beta
!*****************************************************************************80
!
!! beta() is a blockdata routine, but this is the third module with this name.
!
  real x
  common / smith / x
  save / smith /
  data x / 1.0 /
end
