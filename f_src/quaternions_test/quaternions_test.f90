program main

!*****************************************************************************80
!
!! MAIN is the main program for QUATERNIONS_TEST.
!
!  Discussion:
!
!    QUATERNIONS_TEST tests the QUATERNIONS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QUATERNIONS_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the QUATERNIONS library.'

  call q8_conjugate_test ( )
  call q8_exponentiate_test ( )
  call q8_inverse_test ( )
  call q8_multiply_test ( )
  call q8_multiply2_test ( )
  call q8_norm_test ( )
  call q8_normal_01_test ( )
  call q8_transpose_print_test ( )

  call r8_acos_test ( )

  call r8mat_print_test ( )
  call r8mat_print_some_test ( )

  call r8vec_print_test ( )
  call r8vec_uniform_01_test ( )

  call rotation_axis_vector_test ( )
  call rotation_axis2mat_test ( )
  call rotation_axis2quat_test ( )

  call rotation_mat_vector_test ( )
  call rotation_mat2axis_test ( )
  call rotation_mat2quat_test ( )

  call rotation_quat_vector_test ( )
  call rotation_quat2axis_test ( )
  call rotation_quat2mat_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QUATERNIONS_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine q8_conjugate_test ( )

!*****************************************************************************80
!
!! Q8_CONJUGATE_TEST tests Q8_CONJUGATE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  real ( kind = 8 ) q1(4)
  real ( kind = 8 ) q2(4)

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'Q8_CONJUGATE_TEST'
  write ( *, '(a)' ) '  Q8_CONJUGATE conjugates a quaternion;'

  do i = 1, 5

    call q8_normal_01 ( seed, q1 )
    call q8_conjugate ( q1, q2 )

    write ( *, '(a)' ) ''
    call q8_transpose_print ( q1, '  q1 = q8_normal_01 ( seed ):' )
    call q8_transpose_print ( q2, '  q2 = q8_conjugate ( q1 ):  ' )

  end do

  return
end
subroutine q8_exponentiate_test ( )

!*****************************************************************************80
!
!! Q8_EXPONENTIATE_TEST tests Q8_EXPONENTIATE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) q1(4)
  real ( kind = 8 ) q2(4)
  integer ( kind = 4 ) seed

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'Q8_EXPONENTIATE_TEST'
  write ( *, '(a)' ) '  Q8_EXPONENTIATE exponentiates a quaternion'

  do i = 1, 5

    call q8_normal_01 ( seed, q1 )
    call q8_exponentiate ( q1, q2 )

    write ( *, '(a)' ) ''
    call q8_transpose_print ( q1, '  q1 = q8_normal_01 ( seed ):' )
    call q8_transpose_print ( q2, '  q2 = q8_exponentiate ( q1 ):' )  

  end do

  return
end
subroutine q8_inverse_test ( )

!*****************************************************************************80
!
!! Q8_INVERSE_TEST tests Q8_INVERSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) q1(4)
  real ( kind = 8 ) q2(4)
  real ( kind = 8 ) q3(4)
  integer ( kind = 4 ) seed

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'Q8_INVERSE_TEST'
  write ( *, '(a)' ) '  Q8_INVERSE inverts a quaternion'

  do i = 1, 5

    call q8_normal_01 ( seed, q1 )
    call q8_inverse ( q1, q2 )
    call q8_multiply ( q1, q2, q3 )

    write ( *, '(a)' ) ''
    call q8_transpose_print ( q1, '  q1 = q8_normal_01 ( seed ):' )
    call q8_transpose_print ( q2, '  q2 = q8_inverse ( q1 ):    ' )  
    call q8_transpose_print ( q3, '  q3 = q8_multiply ( q1, q2 ):    ' )  

  end do

  return
end
subroutine q8_multiply_test ( )

!*****************************************************************************80
!
!! Q8_MULTIPLY_TEST tests Q8_MULTIPLY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) q1(4)
  real ( kind = 8 ) q2(4)
  real ( kind = 8 ) q3(4)
  integer ( kind = 4 ) seed

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'Q8_MULTIPLY_TEST'
  write ( *, '(a)' ) '  Q8_MULTIPLY multiplies two quaternions'

  do i = 1, 5

    call q8_normal_01 ( seed, q1 )
    call q8_normal_01 ( seed, q2 )
    call q8_multiply ( q1, q2, q3 )

    write ( *, '(a)' ) ''
    call q8_transpose_print ( q1, '  q1 = q8_normal_01 ( seed ) :' )
    call q8_transpose_print ( q2, '  q2 = q8_normal_01 ( seed ) :' )
    call q8_transpose_print ( q3, '  q3 = q8_multiply ( q1, q2 ):' )  

  end do

  return
end
subroutine q8_multiply2_test ( )

!*****************************************************************************80
!
!! Q8_MULTIPLY2_TEST tests Q8_MULTIPLY2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) q1(4)
  real ( kind = 8 ) q2(4)
  real ( kind = 8 ) q3(4)
  integer ( kind = 4 ) seed

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'Q8_MULTIPLY2_TEST'
  write ( *, '(a)' ) '  Q8_MULTIPLY2 multiplies two quaternions using a matrix'

  do i = 1, 5

    call q8_normal_01 ( seed, q1 )
    call q8_normal_01 ( seed, q2 )
    call q8_multiply2 ( q1, q2, q3 )

    write ( *, '(a)' ) ''
    call q8_transpose_print ( q1, '  q1 = q8_normal_01 ( seed )  :' )
    call q8_transpose_print ( q2, '  q2 = q8_normal_01 ( seed )  :' )
    call q8_transpose_print ( q3, '  q3 = q8_multiply2 ( q1, q2 ):' )  

  end do

  return
end
subroutine q8_normal_01_test ( )

!*****************************************************************************80
!
!! Q8_NORMAL_01_TEST tests Q8_NORMAL_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  character ( len = 80 ) label
  real ( kind = 8 ) q(4)
  integer ( kind = 4 ) seed

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'Q8_NORMAL_01_TEST'
  write ( *, '(a)' ) '  Q8_NORMAL_01 computes a normally distributed quaternion.'
  write ( *, '(a)' ) ''

  do i = 1, 5
    call q8_normal_01 ( seed, q )
    write ( label, '(a,i2)' ) '  Sample #', i
    call q8_transpose_print ( q, label )
  end do

  return
end
subroutine q8_norm_test ( )

!*****************************************************************************80
!
!! Q8_NORM_TEST tests Q8_NORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) q(4)
  real ( kind = 8 ) q8_norm
  integer ( kind = 4 ) seed
  real ( kind = 8 ) value

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'Q8_NORM_TEST'
  write ( *, '(a)' ) '  Q8_NORM computes the norm of a quaternion.'

  do i = 1, 5
    write ( *, '(a)' ) ''
    call q8_normal_01 ( seed, q )
    call q8_transpose_print ( q, '  q = q8_normal_01(seed):' )
    value = q8_norm ( q )
    write ( *, '(a,g14.6)' ) '  q8_norm(q) = ', value 
  end do

  return
end
subroutine q8_transpose_print_test ( )

!*****************************************************************************80
!
!! Q8_TRANSPOSE_PRINT_TEST tests Q8_TRANSPOSE_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) seed
  real ( kind = 8 ) q(4)

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'Q8_TRANSPOSE_PRINT_TEST'
  write ( *, '(a)' ) '  Q8_TRANSPOSE_PRINT prints a quaternion "transposed",'
  write ( *, '(a)' ) '  that is, writing it as a row vector.'

  call q8_normal_01 ( seed, q )

  call q8_transpose_print ( q, '  The quaternion:' )

  return
end
subroutine r8_acos_test ( )

!*****************************************************************************80
!
!! R8_ACOS_TEST tests R8_ACOS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) c
  real ( kind = 8 ) r8_acos
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ACOS_TEST'
  write ( *, '(a)' ) '  R8_ACOS computes the arc-cosine of an angle.' 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       C            R8_ACOS(C)        ACOS(C)'
  write ( *, '(a)' ) ''

  do test = -1, 13

    c = real ( test - 6, kind = 8 ) / real ( 6, kind = 8 )

    if ( -1.0D+00 <= c .and. c <= 1.0D+00 ) then
      write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) &
        c, r8_acos ( c ), acos ( c )
    else
      write ( *, '(2x,g14.6,2x,g14.6)' ) &
        c, r8_acos ( c )
    end if

  end do

  return
end
subroutine r8mat_print_test ( )

!*****************************************************************************80
!
!! R8MAT_PRINT_TEST tests R8MAT_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_PRINT_TEST'
  write ( *, '(a)' ) '  R8MAT_PRINT prints an R8MAT.'

  do j = 1, n
    do i = 1, m
      a(i,j) = real ( 10 * i + j, kind = 8 )
    end do
  end do

  call r8mat_print ( m, n, a, '  The R8MAT:' )

  return
end
subroutine r8mat_print_some_test ( )

!*****************************************************************************80
!
!! R8MAT_PRINT_SOME_TEST tests R8MAT_PRINT_SOME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  R8MAT_PRINT_SOME prints some of an R8MAT.'

  do j = 1, n
    do i = 1, m
      a(i,j) = real ( 10 * i + j, kind = 8 )
    end do
  end do

  call r8mat_print_some ( m, n, a, 2, 1, 4, 2, &
    '  The R8MAT, rows 2:4, cols 1:2:' )

  return
end
subroutine r8vec_print_test ( )

!*****************************************************************************80
!
!! R8VEC_PRINT_TEST tests R8VEC_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ), dimension ( n ) :: a = (/ &
    123.456D+00, 0.000005D+00, -1.0D+06, 3.14159265D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_PRINT_TEST'
  write ( *, '(a)' ) '  R8VEC_PRINT prints an R8VEC.'

  call r8vec_print ( n, a, '  The R8VEC:' )

  return
end
subroutine r8vec_uniform_01_test ( )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01_TEST tests R8VEC_UNIFORM_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20

  real ( kind = 8 ) r(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_UNIFORM_01_TEST'
  write ( *, '(a)' ) '  R8VEC_UNIFORM_01 returns a random R8VEC '
  write ( *, '(a)' ) '  with entries in [0,1].'

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a,i12)' ) '  Input SEED = ', seed

  call r8vec_uniform_01 ( n, seed, r )

  call r8vec_print ( n, r, '  Random R8VEC:' )

  return
end
subroutine rotation_axis2mat_test ( )

!*****************************************************************************80
!
!! ROTATION_AXIS2MAT_TEST tests ROTATION_AXIS2MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a(3,3)
  real ( kind = 8 ) angle
  real ( kind = 8 ) axis(3)
  real ( kind = 8 ) degrees_to_radians
  real ( kind = 8 ) v(3)
  real ( kind = 8 ) w(3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ROTATION_AXIS2MAT_TEST'
  write ( *, '(a)' ) '  ROTATION_AXIS2MAT converts a rotation axis to a matrix.'

  v = (/ 1.0D+00, 4.0D+00, 10.0D+00 /)
  call r8vec_print ( 3, v, '  The vector V:' )

  axis = (/ 0.2361737D+00, -0.8814124D+00, -0.4090649D+00 /)
  call r8vec_print ( 3, axis, '  The rotation axis:' )

  angle = 1.159804D+00
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  The rotation angle is ', angle

  call rotation_axis2mat ( axis, angle, a )

  call r8mat_print ( 3, 3, a, '  The rotation matrix A:' )

  w = matmul ( a, v )

  call r8vec_print ( 3, w, '  The rotated vector W = A * V:' )
!
!  Test an axis vector that does not have unit length.
!
  v = (/ 1.0D+00, 1.0D+00, 1.0D+00 /)
  call r8vec_print ( 3, v, '  The vector V:' )

  axis = (/ 0.0D+00, 0.0D+00, 2.0D+00 /)
  call r8vec_print ( 3, axis, '  The rotation axis:' )

  angle = 90.0D+00
  angle = degrees_to_radians ( angle )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  The rotation angle is ', angle

  call rotation_axis2mat ( axis, angle, a )

  call r8mat_print ( 3, 3, a, '  The rotation matrix A:' )

  w = matmul ( a, v )

  call r8vec_print ( 3, w, '  The rotated vector W = A * V:' )

  return
end
subroutine rotation_axis2quat_test ( )

!*****************************************************************************80
!
!! ROTATION_AXIS2QUAT_TEST tests ROTATION_AXIS2QUAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) angle
  real ( kind = 8 ) axis(3)
  real ( kind = 8 ) degrees_to_radians
  real ( kind = 8 ) q(4)
  real ( kind = 8 ) v(3)
  real ( kind = 8 ) w(3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ROTATION_AXIS2QUAT_TEST'
  write ( *, '(a)' ) '  ROTATION_AXIS2QUAT converts a rotation axis to a quaternion.'
 
  v = (/ 1.0D+00, 4.0D+00, 10.0D+00 /)
  call r8vec_print ( 3, v, '  The vector V:' )

  axis = (/ 0.2361737D+00, -0.8814124D+00, -0.4090649D+00 /)
  call r8vec_print ( 3, axis, '  The rotation axis:' )

  angle = 1.159804D+00
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  The rotation angle is ', angle

  call rotation_axis2quat ( axis, angle, q )

  call r8vec_print ( 4, q, '  The rotation quaternion Q:' )

  call rotation_quat_vector ( q, v, w )

  call r8vec_print ( 3, w, '  The rotated vector W:' )
!
!  Another test of ROTATION_AXIS_VECTOR with an axis vector
!  that does not have unit length.
!
  v = (/ 1.0D+00, 1.0D+00, 1.0D+00 /)
  call r8vec_print ( 3, v, '  The vector V:' )

  axis = (/ 0.0D+00, 0.0D+00, 2.0D+00 /)
  call r8vec_print ( 3, axis, '  The rotation axis:' )

  angle = 90.0D+00
  angle = degrees_to_radians ( angle )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  The rotation angle is ', angle
  call rotation_axis2quat ( axis, angle, q )

  call r8vec_print ( 4, q, '  The rotation quaternion Q:' )

  call rotation_quat_vector ( q, v, w )

  call r8vec_print ( 3, w, '  The rotated vector W:' )

  return
end
subroutine rotation_axis_vector_test ( )

!*****************************************************************************80
!
!! ROTATION_AXIS_VECTOR_TEST tests ROTATION_AXIS_VECTOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) angle
  real ( kind = 8 ) axis(3)
  real ( kind = 8 ) degrees_to_radians
  real ( kind = 8 ) v(3)
  real ( kind = 8 ) w(3)

  angle = 1.159804D+00
  axis = (/ 0.2361737D+00, -0.8814124D+00, -0.4090649D+00 /)
  v = (/ 1.0D+00, 4.0D+00, 10.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ROTATION_AXIS_VECTOR_TEST'
  write ( *, '(a)' ) '  ROTATION_AXIS_VECTOR applies an axis'
  write ( *, '(a)' ) '  rotation to a vector.'

  call r8vec_print ( 3, v, '  The vector:' )

  call r8vec_print ( 3, axis, '  The rotation axis:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  The rotation angle is ', angle

  call rotation_axis_vector ( axis, angle, v, w )

  call r8vec_print ( 3, w, '  The rotated vector:' )
!
!  Another test of ROTATION_AXIS_VECTOR with an axis vector
!  that does not have unit length.
!
  v = (/ 1.0D+00, 1.0D+00, 1.0D+00 /)

  call r8vec_print ( 3, v, '  The vector:' )

  axis = (/ 0.0D+00, 0.0D+00, 2.0D+00 /)

  call r8vec_print ( 3, axis, '  The rotation axis:' )

  angle = 90.0D+00
  angle = degrees_to_radians ( angle )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  The rotation angle is ', angle

  call rotation_axis_vector ( axis, angle, v, w )

  call r8vec_print ( 3, w, '  The rotated vector:' )

  return
end
subroutine rotation_mat2axis_test ( )

!*****************************************************************************80
!
!! ROTATION_MAT2AXIS_TEST tests ROTATION_MAT2AXIS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a(3,3)
  real ( kind = 8 ) angle
  real ( kind = 8 ) axis(3)

  a = reshape ( (/ &
    0.43301269D+00, -0.5D+00,        0.75D+00, &
    0.25D+00,        0.86602539D+00, 0.43301269D+00, &
   -0.86602539D+00,  0.0D+00,        0.5D+00 /), (/ 3, 3 /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ROTATION_MAT2AXIS_TEST'
  write ( *, '(a)' ) '  ROTATION_MAT2AXIS computes a rotation axis'
  write ( *, '(a)' ) '  and angle from a rotation matrix.'
  write ( *, '(a)' ) '  ROTATION_AXIS2MAT computes a rotation matrix'
  write ( *, '(a)' ) '  from a rotation axis and angle.'

  call r8mat_print ( 3, 3, a, '  The rotation matrix:' )

  call rotation_mat2axis ( a, axis, angle )

  call r8vec_print ( 3, axis, '  The rotation axis:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  The rotation angle is ', angle

  call rotation_axis2mat ( axis, angle, a )

  call r8mat_print ( 3, 3, a, '  The recovered rotation matrix:' )

  return
end
subroutine rotation_mat2quat_test ( )

!*****************************************************************************80
!
!! ROTATION_MAT2QUAT_TEST tests ROTATION_MAT2QUAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a(3,3)
  real ( kind = 8 ) q(4)

  a = reshape ( (/ &
    0.43301269D+00, -0.5D+00,        0.75D+00, &
    0.25D+00,        0.86602539D+00, 0.43301269D+00, &
   -0.86602539D+00,  0.0D+00,        0.5D+00 /), (/ 3, 3 /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ROTATION_MAT2QUAT_TEST'
  write ( *, '(a)' ) '  ROTATION_MAT2QUAT computes a quaternion'
  write ( *, '(a)' ) '  from a rotation matrix.'
  write ( *, '(a)' ) '  ROTATION_QUAT2MAT computes a rotation matrix'
  write ( *, '(a)' ) '  from a quaternion.'

  call r8mat_print ( 3, 3, a, '  The rotation matrix:' )

  call rotation_mat2quat ( a, q )

  call r8vec_print ( 4, q, '  The rotation quaternion Q:' )

  call rotation_quat2mat ( q, a )

  call r8mat_print ( 3, 3, a, '  The recovered rotation matrix:' )

  return
end
subroutine rotation_mat_vector_test ( )

!*****************************************************************************80
!
!! ROTATION_MAT_VECTOR_TEST tests ROTATION_MAT_VECTOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a(3,3)
  real ( kind = 8 ) v(3)
  real ( kind = 8 ) w(3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ROTATION_MAT_VECTOR_TEST'
  write ( *, '(a)' ) '  ROTATION_MAT_VECTOR applies a matrix'
  write ( *, '(a)' ) '  rotation to a vector.'
 
  a = reshape ( (/ &
    0.43301269D+00, -0.5D+00,        0.75D+00, &
    0.25D+00,        0.86602539D+00, 0.43301269D+00, &
   -0.86602539D+00,  0.0D+00,        0.5D+00 /), (/ 3, 3 /) )

  call r8mat_print ( 3, 3, a, '  The rotation matrix:' )

  v = (/ 1.0D+00, 4.0D+00, 10.0D+00 /)
  call r8vec_print ( 3, v, '  The vector V:' )

  call rotation_mat_vector ( a, v, w )
  call r8vec_print ( 3, w, '  The rotated vector W = A * V:' )

  return
end
subroutine rotation_quat2axis_test ( )

!*****************************************************************************80
!
!! ROTATION_QUAT2AXIS_TEST tests ROTATION_QUAT2AXIS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) angle
  real ( kind = 8 ) axis(3)
  real ( kind = 8 ) q(4)

  q = (/ 0.836516, 0.12941, -0.482963, -0.224144 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ROTATION_QUAT2AXIS_TEST'
  write ( *, '(a)' ) '  ROTATION_QUAT2AXIS computes a rotation axis'
  write ( *, '(a)' ) '  and angle from a rotation quaternion.'
  write ( *, '(a)' ) '  ROTATION_AXIS2QUAT computes a rotation'
  write ( *, '(a)' ) '  quaternion from a rotation axis and angle.'

  call r8vec_print ( 4, q, '  The rotation quaternion:' )

  call rotation_quat2axis ( q, axis, angle )

  call r8vec_print ( 3, axis, '  The rotation axis:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  The rotation angle is ', angle

  call rotation_axis2quat ( axis, angle, q )

  call r8vec_print ( 4, q, '  The recovered rotation quaternion:' )

  return
end
subroutine rotation_quat2mat_test ( )

!*****************************************************************************80
!
!! ROTATION_QUAT2MAT_TEST tests ROTATION_QUAT2MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a(3,3)
  real ( kind = 8 ) q(4)

  q = (/ 0.836516D+00, 0.12941D+00, -0.482963D+00, -0.224144D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ROTATION_QUAT2MAT_TEST'
  write ( *, '(a)' ) '  ROTATION_QUAT2MAT computes a rotation axis'
  write ( *, '(a)' ) '  from a rotation quaternion.'
  write ( *, '(a)' ) '  ROTATION_MAT2QUAT computes a rotation'
  write ( *, '(a)' ) '  quaternion from a rotation matrix.'

  call r8vec_print ( 4, q, '  The rotation quaternion:' )

  call rotation_quat2mat ( q, a )

  call r8mat_print ( 3, 3, a, '  The rotation matrix:' )

  call rotation_mat2quat ( a, q )

  call r8vec_print ( 4, q, '  The recovered rotation quaternion:' )

  return
end
subroutine rotation_quat_vector_test ( )

!*****************************************************************************80
!
!! ROTATION_QUAT_VECTOR_TEST tests ROTATION_QUAT_VECTOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!   04 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) q(4)
  real ( kind = 8 ) v(3)
  real ( kind = 8 ) w(3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ROTATION_QUAT_VECTOR_TEST'
  write ( *, '(a)' ) '  ROTATION_QUAT_VECTOR applies a quaternion'
  write ( *, '(a)' ) '  rotation to a vector.'

  q = (/ 0.836516D+00, 0.12941D+00, -0.482963D+00, -0.224144D+00 /)
  call r8vec_print ( 4, q, '  The rotation quaternion:' )

  v = (/ 1.0D+00, 4.0D+00, 10.0D+00 /)
  call r8vec_print ( 3, v, '  The vector V:' )

  call rotation_quat_vector ( q, v, w )
  call r8vec_print ( 3, w, '  The rotated vector:' )

  return
end

