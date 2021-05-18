subroutine abc ( a_in, b_in, c_in, a_out, b_out, c_out )

!*****************************************************************************80
!
!! abc() stores, saves, and returns varables "a", "b" and "c".
!
!  Discussion:
!
!    Calling abc() with no input arguments returns the current
!    values of A, B, and C.
!
!    Calling abc(a_in,b_in,c_in) supplies new values for A, B,
!    and C which overwrite the current values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 May 2021
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real A_IN: a new value for A.
!
!    real B_IN: a new value for B.
!
!    real C_IN: a new value for C.
!
!  Persistent:
!
!    real A_DEFAULT: the current value of A.
!
!    real B_DEFAULT: the current value of B.
!
!    real C_DEFAULT: the current value of C.
!
!  Output:
!
!    real A_OUT: the current value of A.
!
!    real B_OUT: the current value of B.
!
!    real C_OUT: the current value or C.
!
  implicit none

  real ( kind = 8 ), save     :: a_default = 1.0D+00
  real ( kind = 8 ), optional :: a_in
  real ( kind = 8 ), optional :: a_out
  real ( kind = 8 ), save     :: b_default = 2.0D+00
  real ( kind = 8 ), optional :: b_in
  real ( kind = 8 ), optional :: b_out
  real ( kind = 8 ), save     :: c_default = 3.0D+00
  real ( kind = 8 ), optional :: c_in
  real ( kind = 8 ), optional :: c_out
!
!  New values, if supplied on input, overwrite the current values.
!
  if ( present ( a_in ) ) then
    a_default = a_in;
  end if

  if ( present ( b_in ) ) then
    b_default = b_in;
  end if

  if ( present ( c_in ) ) then
    c_default = c_in;
  end if
!
!  The current values are copied to the output.
!
  if ( present ( a_out ) ) then
    a_out = a_default;
  end if

  if ( present ( b_out ) ) then
    b_out = b_default;
  end if

  if ( present ( c_out ) ) then
    c_out = c_default;
  end if

  return
end

