program main

!*****************************************************************************80
!
!! MAIN is the main program for triangle_type_test.
!
!  Discussion:
!
!    This program shows how the user may create a defined data type.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none
!
!  This definition must be included in any function or subroutine that wishes
!  to work with the triangle data type.
!
  type triangle
    real ( kind = 8 ) :: v(2,3)
    real ( kind = 8 ) :: area
  end type

  type ( triangle ) :: t
  real ( kind = 8 ) triangle_area

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'triangle_type_test:'
  write ( *, '(a)' ) '  Demonstrate how to create a derived data type.'
  write ( *, '(a)' ) '  The TYPE statement names the type, and components.'
  write ( *, '(a)' ) '  The percent sign is used to access a component.'

  t%v(1,1) = 0.0
  t%v(2,1) = 0.0
  
  t%v(1,2) = 1.0
  t%v(2,2) = 0.0
  
  t%v(1,3) = 0.0
  t%v(2,3) = 2.0

  t%area = triangle_area ( t )

  call triangle_print ( t )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'triangle_type_test:'
  write ( *, '(a)' ) '  Normal end of execution.'

  stop 0
end
function triangle_area ( t )

!*****************************************************************************80
!
!! TRIANGLE_AREA computes the area of a triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, triangle T, the triangle information.
!
!    Output, real ( kind = 8 ) TRIANGLE_AREA, the area of the triangle.
!
  implicit none

  type triangle
    real ( kind = 8 ) :: v(2,3)
    real ( kind = 8 ) :: area
  end type

  type ( triangle ) :: t
  real ( kind = 8 ) triangle_area

  triangle_area = 0.5 * ( &
      t%v(1,1) * ( t%v(2,2) - t%v(2,3) ) &
    + t%v(1,2) * ( t%v(2,3) - t%v(2,1) ) &
    + t%v(1,3) * ( t%v(2,1) - t%v(2,2) ) )

  return
end
subroutine triangle_print ( t )

!*****************************************************************************80
!
!! TRIANGLE_PRINT prints information about a triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, triangle T, the triangle information.
!
  implicit none

  type triangle
    real ( kind = 8 ) :: v(2,3)
    real ( kind = 8 ) :: area
  end type

  integer ( kind = 4 ) j
  type ( triangle ) :: t

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Triangle information:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Vertices:'
  write ( *, '(a)' ) ''
  do j = 1, 3
    write ( *, '(2g14.6)' ) t%v(1:2,j)
  end do
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Area = ', t%area

  return
end
