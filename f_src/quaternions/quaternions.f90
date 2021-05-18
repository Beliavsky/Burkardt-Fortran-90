function degrees_to_radians ( angle_deg )

!*****************************************************************************80
!
!! DEGREES_TO_RADIANS converts an angle from degrees to radians.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ANGLE_DEG, an angle in degrees.
!
!    Output, real ( kind = 8 ) DEGREES_TO_RADIANS, the equivalent angle
!    in radians.
!
  implicit none

  real ( kind = 8 ) angle_deg
  real ( kind = 8 ) degrees_to_radians
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00

  degrees_to_radians = ( angle_deg / 180.0D+00 ) * r8_pi

  return
end
subroutine q8_conjugate ( q, q2 )

!*****************************************************************************80
!
!! Q8_CONJUGATE conjugates a quaternion.
!
!  Discussion:
!
!    A quaternion is a quadruplet (A,B,C,D) of real numbers, which
!    may be written as
!
!      Q = A + Bi + Cj + Dk.
!
!    The conjugate of Q is
!
!      conj ( Q ) = A - Bi - Cj - Dk.
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
!  Parameters:
!
!    Input, real ( kind = 8 ) Q(4), the quaternion to be conjugated.
!
!    Output, real ( kind = 8 ) Q2(4), the conjugated quaternion.
!
  implicit none

  real ( kind = 8 ) q(4)
  real ( kind = 8 ) q2(4)

  q2(1) =  q(1)
  q2(2:3) = -q(2:3)

  return
end
subroutine q8_exponentiate ( q1, q2 )

!*****************************************************************************80
!
!! Q8_EXPONENTIATE exponentiates a quaternion.
!
!  Discussion:
!
!    A quaternion is a quadruplet (A,B,C,D) of real numbers, which
!    may be written as
!
!      Q = A + Bi + Cj + Dk.
!    
!    The exponential of Q can be set by
!      V = sqrt ( B^2 + C^2 + D^2 )
!      e^Q = e^A * ( cos ( ||V|| ) + V/||V|| sin ||V|| )
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
!  Parameters:
!
!    Input, real ( kind = 8 ) Q1(4), the quaternions to exponentiate.
!
!    Output, real ( kind = 8 ) Q2(4), the exponential of the quaternion.
!
  implicit none

  real ( kind = 8 ) q1(4)
  real ( kind = 8 ) q2(4)
  real ( kind = 8 ) v(3)
  real ( kind = 8 ) v_norm

  v = q1(2:4)
  v_norm = sqrt ( sum ( v(1:3) ** 2 ) )

  q2(1) = cos ( v_norm )
  if ( v_norm /= 0.0D+00 ) then
    q2(2:4) = sin ( v_norm ) * v / v_norm
  else
    q2(2:4) = 0.0D+00
  end if

  q2 = exp ( q1(1) ) * q2

  return
end
subroutine q8_inverse ( q, q2 )

!*****************************************************************************80
!
!! Q8_INVERSE inverts a quaternion.
!
!  Discussion:
!
!    A quaternion is a quadruplet (A,B,C,D) of real numbers, which
!    may be written as
!
!      Q = A + Bi + Cj + Dk.
!
!    The inverse of Q is
!
!      inverse ( Q ) = conjugate ( Q ) / ( norm ( Q ) )^2.
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
!  Parameters:
!
!    Input, real ( kind = 8 ) Q(4), the quaternion to be inverted.
!
!    Output, real ( kind = 8 ) Q2(4), the inverse of the input quaternion.
!
  implicit none

  real ( kind = 8 ) q(4)
  real ( kind = 8 ) q2(4)

  q2(1:4) = q(1:4) / sum ( q(1:4) ** 2 ) 
  q2(2:4) = - q2(2:4)

  return
end
subroutine q8_multiply ( q1, q2, q3 )

!*****************************************************************************80
!
!! Q8_MULTIPLY multiplies two quaternions.
!
!  Discussion:
!
!    A quaternion is a quadruplet (A,B,C,D) of real numbers, which
!    may be written as
!
!      Q = A + Bi + Cj + Dk.
!
!    To multiply two quaternions, use the relationships:
!
!      i * j = -j * i = k
!      j * k = -k * j = i
!      k * i = -i * k = j
!      i * i =  j * j = k * k = -1
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
!  Parameters:
!
!    Input, real ( kind = 8 ) Q1(4), Q2(4), the quaternions to be multiplied.
!
!    Output, real ( kind = 8 ) Q3(4), the product of the two quaternions.
!
  implicit none

  real ( kind = 8 ) q1(4)
  real ( kind = 8 ) q2(4)
  real ( kind = 8 ) q3(4)

  q3(1) = q1(1) * q2(1) - q1(2) * q2(2) - q1(3) * q2(3) - q1(4) * q2(4)
  q3(2) = q1(1) * q2(2) + q1(2) * q2(1) + q1(3) * q2(4) - q1(4) * q2(3)
  q3(3) = q1(1) * q2(3) - q1(2) * q2(4) + q1(3) * q2(1) + q1(4) * q2(2)
  q3(4) = q1(1) * q2(4) + q1(2) * q2(3) - q1(3) * q2(2) + q1(4) * q2(1)

  return
end
subroutine q8_multiply2 ( q1, q2, q3 )

!*****************************************************************************80
!
!! Q8_MULTIPLY2 multiplies two quaternions using a matrix format.
!
!  Discussion:
!
!    A quaternion is a quadruplet (A,B,C,D) of real numbers, which
!    may be written as
!
!      Q = A + Bi + Cj + Dk.
!
!    To multiply two quaternions, use the relationships:
!
!      i * j = -j * i = k
!      j * k = -k * j = i
!      k * i = -i * k = j
!      i * i =  j * j = k * k = -1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 July 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) Q1(4), Q2(4), the quaternions to be multiplied.
!
!    Output, real ( kind = 8 ) Q3(4), the product of the two quaternions.
!
  implicit none

  real ( kind = 8 ) q1(4)
  real ( kind = 8 ) q2(4)
  real ( kind = 8 ) q3(4)
  real ( kind = 8 ) qm(4,4)
!
!  The matrix entries are listed by column, not row.
!
  qm = reshape ( (/ &
    q1(1),  q1(2),  q1(3),  q1(4), &
   -q1(2), +q1(1), +q1(4), -q1(3), &
   -q1(3), -q1(4), +q1(1), +q1(2), & 
   -q1(4), +q1(3), -q1(2), +q1(1) /), (/ 4, 4 /) )

  q3 = matmul ( qm, q2 )
 
  return
end
function q8_norm ( q )

!*****************************************************************************80
!
!! Q8_NORM computes the norm of a quaternion.
!
!  Discussion:
!
!    A quaternion is a quadruplet (A,B,C,D) of real numbers, which
!    may be written as
!
!      Q = A + Bi + Cj + Dk.
!
!    The norm of Q is
!
!      norm ( Q ) = sqrt ( A * A + B * B + C * C + D * D ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 August 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) Q(4), the quaternion.
!
!    Output, real ( kind = 8 ) Q8_NORM, the norm of the quaternion.
!
  implicit none

  real ( kind = 8 ) q(4)
  real ( kind = 8 ) q8_norm

  q8_norm = sqrt ( sum ( q(1:4) ** 2 ) )

  return
end
subroutine q8_normal_01 ( seed, q )

!*****************************************************************************80
!
!! Q8_NORMAL_01 returns a normally distributed quaternion.
!
!  Discussion:
!
!    The normal distribution with mean 0 and variance 1 is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 July 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number 
!    generator.
!
!    Output, real ( kind = 8 ) Q(4), the sampled quaternion.
!
  implicit none

  integer ( kind = 4 ) seed
  real ( kind = 8 ) q(4)
  real ( kind = 8 ) r(4)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00

  call r8vec_uniform_01 ( 4, seed, r )

  q(1:3:2) = &
    sqrt ( - 2.0D+00 * log ( r(1:3:2) ) ) * cos ( 2.0D+00 * r8_pi * r(2:4:2) )

  q(2:4:2) = &
    sqrt ( - 2.0D+00 * log ( r(1:3:2) ) ) * sin ( 2.0D+00 * r8_pi * r(2:4:2) )

  return
end
subroutine q8_transpose_print ( q, title )

!*****************************************************************************80
!
!! Q8_TRANSPOSE_PRINT prints a Q8 "transposed".
!
!  Discussion:
!
!    A Q8 is a quaternion using R8 arithmetic.
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
!  Parameters:
!
!    Input, real ( kind = 8 ) Q(4), the quaternion to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  real ( kind = 8 ) q(4)
  character ( len = * ) title

  write ( *, '(a,2x,4g16.8)' ) trim ( title ), q(1:4)
 
  return
end
function r8_acos ( c )

!*****************************************************************************80
!
!! R8_ACOS computes the arc cosine function, with argument truncation.
!
!  Discussion:
!
!    If you call your system ACOS routine with an input argument that is
!    even slightly outside the range [-1.0, 1.0 ], you may get an unpleasant
!    surprise (I did).
!
!    This routine simply truncates arguments outside the range.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) C, the argument.
!
!    Output, real ( kind = 8 ) R8_ACOS, an angle whose cosine is C.
!
  implicit none

  real ( kind = 8 ) c
  real ( kind = 8 ) c2
  real ( kind = 8 ) r8_acos

  c2 = c
  c2 = max ( c2, -1.0D+00 )
  c2 = min ( c2, +1.0D+00 )

  r8_acos = acos ( c2 )

  return
end
subroutine r8mat_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_PRINT prints an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows in A.
!
!    Input, integer ( kind = 4 ) N, the number of columns in A.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = * ) title

  call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8MAT_PRINT_SOME prints some of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

  do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i8,6x)' ) j
    end do

    write ( *, '(''  Col   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) ' '

    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, m )

    do i = i2lo, i2hi

      do j2 = 1, inc

        j = j2lo - 1 + j2

        write ( ctemp(j2), '(g14.6)' ) a(i,j)

      end do

      write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

    end do

  end do

  return
end
subroutine r8vec_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_PRINT prints an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
  end do

  return
end
subroutine r8vec_uniform_01 ( n, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    r(i) = real ( seed, kind = 8 ) * 4.656612875D-10

  end do

  return
end
function radians_to_degrees ( angle_rad )

!*****************************************************************************80
!
!! RADIANS_TO_DEGREES converts an angle from radians to degrees.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ANGLE_RAD, an angle in radians.
!
!    Output, real ( kind = 8 ) RADIANS_TO_DEGREES, the equivalent angle
!    in degrees.
!
  implicit none

  real ( kind = 8 ) angle_rad
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) radians_to_degrees

  radians_to_degrees = ( angle_rad / r8_pi ) * 180.0D+00

  return
end
subroutine rotation_axis_vector ( axis, angle, v, w )

!*****************************************************************************80
!
!! ROTATION_AXIS_VECTOR rotates a vector around an axis vector in 3D.
!
!  Discussion:
!
!    Thanks to Cody Farnell for correcting some mistakes in an earlier
!    version of this routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) AXIS(3), the axis vector for the rotation.
!
!    Input, real ( kind = 8 ) ANGLE, the angle, in radians, of the rotation.
!
!    Input, real ( kind = 8 ) V(3), the vector to be rotated.
!
!    Output, real ( kind = 8 ) W(3), the rotated vector.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  real ( kind = 8 ) angle
  real ( kind = 8 ) axis(dim_num)
  real ( kind = 8 ) axis_norm
  real ( kind = 8 ) dot
  real ( kind = 8 ) norm
  real ( kind = 8 ) normal(dim_num)
  real ( kind = 8 ) normal_component
  real ( kind = 8 ) normal2(dim_num)
  real ( kind = 8 ) parallel(dim_num)
  real ( kind = 8 ) rot(dim_num)
  real ( kind = 8 ) u(dim_num)
  real ( kind = 8 ) v(dim_num)
  real ( kind = 8 ) w(dim_num)
!
!  Compute the length of the rotation axis.
!
  u(1:dim_num) = axis(1:dim_num)

  axis_norm = sqrt ( sum ( u(1:dim_num) ** 2 ) )

  if ( axis_norm == 0.0D+00 ) then
    w(1:dim_num) = 0.0D+00
    return
  end if

  u(1:dim_num) = u(1:dim_num) / axis_norm
!
!  Compute the dot product of the vector and the unit rotation axis.
!
  dot = dot_product ( u(1:dim_num), v(1:dim_num) )
!
!  Compute the parallel component of the vector.
!
  parallel(1:dim_num) = dot * u(1:dim_num)
!
!  Compute the normal component of the vector.
!
  normal(1:dim_num) = v(1:dim_num) - parallel(1:dim_num)

  normal_component = sqrt ( sum ( normal(1:dim_num) ** 2 ) )

  if ( normal_component == 0.0D+00 ) then
    w(1:dim_num) = parallel(1:dim_num)
    return
  end if

  normal(1:dim_num) = normal(1:dim_num) / normal_component
!
!  Compute a second vector, lying in the plane, perpendicular
!  to V, and forming a right-handed system, as the cross product
!  of the first two vectors.
!
  normal2(1) = u(2) * normal(3) - u(3) * normal(2)
  normal2(2) = u(3) * normal(1) - u(1) * normal(3)
  normal2(3) = u(1) * normal(2) - u(2) * normal(1)

  norm = sqrt ( sum ( normal2(1:dim_num) ** 2 ) )

  normal2(1:dim_num) = normal2(1:dim_num) / norm
!
!  Rotate the normal component by the angle.
!
  rot(1:dim_num) = normal_component * ( &
      cos ( angle ) * normal(1:dim_num) &
    + sin ( angle ) * normal2(1:dim_num) )
!
!  The rotated vector is the parallel component plus the rotated component.
!
  w(1:dim_num) = parallel(1:dim_num) + rot(1:dim_num)

  return
end
subroutine rotation_axis2mat ( axis, angle, a )

!*****************************************************************************80
!
!! ROTATION_AXIS2MAT converts a rotation from axis to matrix format in 3D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    James Foley, Andries van Dam, Steven Feiner, John Hughes,
!    Computer Graphics, Principles and Practice,
!    Second Edition,
!    Addison Wesley, 1990.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) AXIS(3), the axis vector which remains 
!    unchanged by the rotation.
!
!    Input, real ( kind = 8 ) ANGLE, the angular measurement of the
!    rotation about the axis, in radians.
!
!    Output, real ( kind = 8 ) A(3,3), the rotation matrix.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  real ( kind = 8 ) a(dim_num,dim_num)
  real ( kind = 8 ) angle
  real ( kind = 8 ) axis(dim_num)
  real ( kind = 8 ) axis_norm
  real ( kind = 8 ) ca
  real ( kind = 8 ) sa
  real ( kind = 8 ) v1
  real ( kind = 8 ) v2
  real ( kind = 8 ) v3

  v1 = axis(1)
  v2 = axis(2)
  v3 = axis(3)

  axis_norm = sqrt ( sum ( axis(1:dim_num) ** 2 ) )

  if ( axis_norm == 0.0D+00 ) then
    a(1:dim_num,1:dim_num) = 0.0D+00
    return
  end if

  v1 = v1 / axis_norm
  v2 = v2 / axis_norm
  v3 = v3 / axis_norm

  ca = cos ( angle )
  sa = sin ( angle )

  a(1,1) =                    v1 * v1 + ca * ( 1.0D+00 - v1 * v1 )
  a(1,2) = ( 1.0D+00 - ca ) * v1 * v2 - sa * v3
  a(1,3) = ( 1.0D+00 - ca ) * v1 * v3 + sa * v2

  a(2,1) = ( 1.0D+00 - ca ) * v2 * v1 + sa * v3
  a(2,2) =                    v2 * v2 + ca * ( 1.0D+00 - v2 * v2 )
  a(2,3) = ( 1.0D+00 - ca ) * v2 * v3 - sa * v1

  a(3,1) = ( 1.0D+00 - ca ) * v3 * v1 - sa * v2
  a(3,2) = ( 1.0D+00 - ca ) * v3 * v2 + sa * v1
  a(3,3) =                    v3 * v3 + ca * ( 1.0D+00 - v3 * v3 )

  return
end
subroutine rotation_axis2quat ( axis, angle, q )

!*****************************************************************************80
!
!! ROTATION_AXIS2QUAT converts rotation from axis to quaternion form in 3D.
!
!  Discussion:
!
!    A rotation quaternion Q has the form:
!
!      Q = A + Bi + Cj + Dk
!
!    where A, B, C and D are real numbers, and i, j, and k are to be regarded
!    as symbolic constant basis vectors, similar to the role of the "i"
!    in the representation of imaginary numbers.
!
!    A is the cosine of half of the angle of rotation.  (B,C,D) is a
!    unit vector pointing in the direction of the axis of rotation.
!    Rotation multiplication and inversion can be carried out using
!    this format and the usual rules for quaternion multiplication
!    and inversion.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) AXIS(3), the axis vector which remains 
!    unchanged by the rotation.
!
!    Input, real ( kind = 8 ) ANGLE, the angular measurement of the 
!    rotation about the axis, in radians.
!
!    Output, real ( kind = 8 ) Q(4), the quaternion representing the rotation.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  real ( kind = 8 ) axis(dim_num)
  real ( kind = 8 ) axis_norm
  real ( kind = 8 ) angle
  real ( kind = 8 ) q(4)

  axis_norm = sqrt ( sum ( axis(1:dim_num) ** 2 ) )

  if ( axis_norm == 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ROTATION_AXIS2QUAT - Fatal error!'
    write ( *, '(a)' ) '  The axis vector is null.'
    q(1:4) = 0.0D+00
    stop 1
  end if

  q(1)   = cos ( 0.5D+00 * angle )
  q(2:4) = sin ( 0.5D+00 * angle ) * axis(1:3) / axis_norm

  return
end
subroutine rotation_mat_vector ( a, v, w )

!*****************************************************************************80
!
!! ROTATION_MAT_VECTOR applies a marix rotation to a vector in 3d.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(3,3), the matrix defining the rotation.
!
!    Input, real ( kind = 8 ) V(3), the vector to be rotated.
!
!    Output, real ( kind = 8 ) W(3), the rotated vector.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  real ( kind = 8 ) a(dim_num,dim_num)
  real ( kind = 8 ) v(dim_num)
  real ( kind = 8 ) w(dim_num)

  w(1:dim_num) = matmul ( a(1:dim_num,1:dim_num), v(1:dim_num) )

  return
end
subroutine rotation_mat2axis ( a, axis, angle )

!*****************************************************************************80
!
!! ROTATION_MAT2AXIS converts a rotation from matrix to axis format in 3D.
!
!  Discussion:
!
!    The computation is based on the fact that a rotation matrix must
!    have an eigenvector corresponding to the eigenvalue of 1, hence:
!
!      ( A - I ) * v = 0.
!
!    The eigenvector V is the axis of rotation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jack Kuipers,
!    Quaternions and Rotation Sequences,
!    Princeton, 1998.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(3,3), the rotation matrix.
!
!    Output, real ( kind = 8 ) AXIS(3), the axis vector which remains
!    unchanged by the rotation.
!
!    Output, real ( kind = 8 ) ANGLE, the angular measurement of the
!    rotation about the axis, in radians.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  real ( kind = 8 ) a(dim_num,dim_num)
  real ( kind = 8 ) axis(dim_num)
  real ( kind = 8 ) axis_norm
  real ( kind = 8 ) angle
  real ( kind = 8 ) r8_acos
!
!  Compute the normalized axis of rotation.
!
  axis(1) = a(3,2) - a(2,3)
  axis(2) = a(1,3) - a(3,1)
  axis(3) = a(2,1) - a(1,2)

  axis_norm = sqrt ( sum ( axis(1:dim_num) ** 2 ) )

  if ( axis_norm == 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ROTATION_MAT2AXIS - Fatal error!'
    write ( *, '(a)' ) '  A is not a rotation matrix,'
    write ( *, '(a)' ) '  or there are multiple axes of rotation.'
    stop 1
  end if

  axis(1:dim_num) = axis(1:dim_num) / axis_norm
!
!  Find the angle.
!
  angle = r8_acos ( 0.5D+00 * ( a(1,1) + a(2,2) + a(3,3) - 1.0D+00 ) )

  return
end
subroutine rotation_mat2quat ( a, q )

!*****************************************************************************80
!
!! ROTATION_MAT2QUAT converts rotation from matrix to quaternion format.
!
!  Discussion:
!
!    The computation is based on the fact that a rotation matrix must
!    have an eigenvector corresponding to the eigenvalue of 1, hence:
!
!      ( A - I ) * v = 0.
!
!    The eigenvector V is the axis of rotation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jack Kuipers,
!    Quaternions and Rotation Sequences,
!    Princeton, 1998.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(3,3), the rotation matrix.
!
!    Output, real ( kind = 8 ) Q(4), the quaternion representing the rotation.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  real ( kind = 8 ) a(dim_num,dim_num)
  real ( kind = 8 ) angle
  real ( kind = 8 ) axis(dim_num)
  real ( kind = 8 ) axis_norm
  real ( kind = 8 ) cos_phi
  real ( kind = 8 ) q(4)
  real ( kind = 8 ) r8_acos
  real ( kind = 8 ) sin_phi
!
!  Compute the normalized axis of rotation.
!
  axis(1) = a(3,2) - a(2,3)
  axis(2) = a(1,3) - a(3,1)
  axis(3) = a(2,1) - a(1,2)

  axis_norm = sqrt ( sum ( axis(1:dim_num) ** 2 ) )

  if ( axis_norm == 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ROTATION_MAT2QUAT - Fatal error!'
    write ( *, '(a)' ) '  A is not a rotation matrix,'
    write ( *, '(a)' ) '  or there are multiple axes of rotation.'
    stop 1
  end if

  axis(1:dim_num) = axis(1:dim_num) / axis_norm
!
!  Compute the angle.
!
  angle = r8_acos ( 0.5D+00 * ( a(1,1) + a(2,2) + a(3,3) - 1.0D+00 ) )
!
!  Compute the quaternion.
!
  cos_phi = cos ( 0.5D+00 * angle )

  sin_phi = sqrt ( 1.0D+00 - cos_phi * cos_phi )

  q(1)   = cos_phi
  q(2:4) = sin_phi * axis(1:3)

  return
end
subroutine rotation_quat_vector ( q, v, w )

!*****************************************************************************80
!
!! ROTATION_QUAT_VECTOR applies a quaternion rotation to a vector in 3D.
!
!  Discussion:
!
!    If Q is a unit quaternion that encodes a rotation of ANGLE
!    radians about the vector AXIS, then for an arbitrary real
!    vector V, the result W of the rotation on V can be written as:
!
!      W = Q * V * Conj(Q)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) Q(4), the quaternion defining the rotation.
!
!    Input, real ( kind = 8 ) V(3), the vector to be rotated.
!
!    Output, real ( kind = 8 ) W(3), the rotated vector.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  real ( kind = 8 ) q(4)
  real ( kind = 8 ) v(dim_num)
  real ( kind = 8 ) w(dim_num)

  w(1) = &
         ( 2.0D+00 * ( q(1) * q(1) + q(2) * q(2) ) - 1.0D+00 ) * v(1) &
       +   2.0D+00 * ( q(2) * q(3) - q(1) * q(4) )             * v(2) &
       +   2.0D+00 * ( q(2) * q(4) + q(1) * q(3) )             * v(3)

  w(2) = &
           2.0D+00 * ( q(2) * q(3) + q(1) * q(4) )             * v(1) &
       + ( 2.0D+00 * ( q(1) * q(1) + q(3) * q(3) ) - 1.0D+00 ) * v(2) &
       +   2.0D+00 * ( q(3) * q(4) - q(1) * q(2) )             * v(3)

  w(3) = &
           2.0D+00 * ( q(2) * q(4) - q(1) * q(3) )             * v(1) &
       +   2.0D+00 * ( q(3) * q(4) + q(1) * q(2) )             * v(2) &
       + ( 2.0D+00 * ( q(1) * q(1) + q(4) * q(4) ) - 1.0D+00 ) * v(3)

  return
end
subroutine rotation_quat2axis ( q, axis, angle )

!*****************************************************************************80
!
!! ROTATION_QUAT2AXIS converts rotation from quaternion to axis form in 3D.
!
!  Discussion:
!
!    A rotation quaternion Q has the form:
!
!      Q = A + Bi + Cj + Dk
!
!    where A, B, C and D are real numbers, and i, j, and k are to be regarded
!    as symbolic constant basis vectors, similar to the role of the "i"
!    in the representation of imaginary numbers.
!
!    A is the cosine of half of the angle of rotation.  (B,C,D) is a
!    vector pointing in the direction of the axis of rotation.
!    Rotation multiplication and inversion can be carried out using
!    this format and the usual rules for quaternion multiplication
!    and inversion.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) Q(4), the quaternion representing the rotation.
!
!    Output, real ( kind = 8 ) AXIS(3), the axis vector which remains 
!    unchanged by the rotation.
!
!    Output, real ( kind = 8 ) ANGLE, the angular measurement of the 
!    rotation about the axis, in radians.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  real ( kind = 8 ) axis(dim_num)
  real ( kind = 8 ) angle
  real ( kind = 8 ) cos_phi
  real ( kind = 8 ) q(4)
  real ( kind = 8 ) sin_phi

  sin_phi = sqrt ( sum ( q(2:4) ** 2 ) )

  cos_phi = q(1)

  angle = 2.0D+00 * atan2 ( sin_phi, cos_phi )

  if ( sin_phi == 0.0D+00 ) then
    axis(1:dim_num) = (/ 1.0D+00, 0.0D+00, 0.0D+00 /)
  else
    axis(1:dim_num) = q(2:4) / sin_phi
  end if

  return
end
subroutine rotation_quat2mat ( q, a )

!*****************************************************************************80
!
!! ROTATION_QUAT2MAT converts rotation from quaternion to matrix form in 3D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    James Foley, Andries van Dam, Steven Feiner, John Hughes,
!    Computer Graphics, Principles and Practice,
!    Second Edition,
!    Addison Wesley, 1990.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) Q(4), the quaternion representing the rotation.
!
!    Output, real ( kind = 8 ) A(3,3), the rotation matrix.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  real ( kind = 8 ) a(dim_num,dim_num)
  real ( kind = 8 ) angle
  real ( kind = 8 ) ca
  real ( kind = 8 ) cos_phi
  real ( kind = 8 ) q(4)
  real ( kind = 8 ) sa
  real ( kind = 8 ) sin_phi
  real ( kind = 8 ) v1
  real ( kind = 8 ) v2
  real ( kind = 8 ) v3

  sin_phi = sqrt ( sum ( q(2:4) ** 2 ) )

  cos_phi = q(1)

  angle = 2.0D+00 * atan2 ( sin_phi, cos_phi )

  if ( sin_phi == 0.0D+00 ) then
    v1 = 1.0D+00
    v2 = 0.0D+00
    v3 = 0.0D+00
  else
    v1 = q(2) / sin_phi
    v2 = q(3) / sin_phi
    v3 = q(4) / sin_phi
  end if

  ca = cos ( angle )
  sa = sin ( angle )

  a(1,1) =                    v1 * v1 + ca * ( 1.0D+00 - v1 * v1 )
  a(1,2) = ( 1.0D+00 - ca ) * v1 * v2 - sa * v3
  a(1,3) = ( 1.0D+00 - ca ) * v1 * v3 + sa * v2

  a(2,1) = ( 1.0D+00 - ca ) * v2 * v1 + sa * v3
  a(2,2) =                    v2 * v2 + ca * ( 1.0D+00 - v2 * v2 )
  a(2,3) = ( 1.0D+00 - ca ) * v2 * v3 - sa * v1

  a(3,1) = ( 1.0D+00 - ca ) * v3 * v1 - sa * v2
  a(3,2) = ( 1.0D+00 - ca ) * v3 * v2 + sa * v1
  a(3,3) =                    v3 * v3 + ca * ( 1.0D+00 - v3 * v3 )

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
