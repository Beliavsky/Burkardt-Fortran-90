program main

!*****************************************************************************80
!
!! MAIN is the main program for NORMAL_TEST.
!
!  Discussion:
!
!    NORMAL_TEST tests the NORMAL library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version;'
  write ( *, '(a)' ) '  Test the NORMAL library.'

  call c4_normal_01_test ( )
  call c8_normal_01_test ( )
  call i4_normal_ab_test ( )
  call r4_normal_01_test ( )
  call r4_normal_ab_test ( )
  call r4_uniform_01_test ( )
  call r8_normal_01_test ( )
  call r8_normal_ab_test ( )
  call r8_uniform_01_test ( )
  call r8mat_normal_01_test ( )
  call r8mat_normal_ab_test ( )
  call r8vec_normal_01_test ( )
  call r8vec_normal_ab_test ( )
  call r8vec_uniform_01_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine c4_normal_01_test ( )

!*****************************************************************************80
!
!! C4_NORMAL_01_TEST tests C4_NORMAL_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 4 ) c4_normal_01
  integer ( kind = 4 ) i
  complex ( kind = 4 ) r
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C4_NORMAL_01_TEST'
  write ( *, '(a)' ) '  C4_NORMAL_01 computes pseudorandom complex values '
  write ( *, '(a)' ) '  with a Normal 01 circular distribution.'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  SEED = ', seed
  write ( *, '(a)' ) ' '

  do i = 1, 10
    r = c4_normal_01 ( seed )
    write ( *, '(2x,i8,2x,f14.8,2x,f14.8)' ) i, r
  end do

  return
end
subroutine c8_normal_01_test ( )

!*****************************************************************************80
!
!! C8_NORMAL_01_TEST tests C8_NORMAL_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c8_normal_01
  integer ( kind = 4 ) i
  complex ( kind = 8 ) r
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_NORMAL_01_TEST'
  write ( *, '(a)' ) '  C8_NORMAL_01 computes pseudorandom double precision'
  write ( *, '(a)' ) '  complex values with Normal 01 circular distribution.'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  SEED = ', seed
  write ( *, '(a)' ) ' '

  do i = 1, 10
    r = c8_normal_01 ( seed )
    write ( *, '(2x,i8,2x,f14.8,2x,f14.8)' ) i, r
  end do

  return
end
subroutine r4_normal_01_test ( )

!*****************************************************************************80
!
!! R4_NORMAL_01_TEST tests R4_NORMAL_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 4 ) r
  real ( kind = 4 ) r4_normal_01
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R4_NORMAL_01_TEST'
  write ( *, '(a)' ) '  R4_NORMAL_01 computes normal pseudorandom values '
  write ( *, '(a)' ) '  with mean 0.0 and standard deviation 1.0.'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  SEED = ', seed
  write ( *, '(a)' ) ' '

  do i = 1, 10
    r = r4_normal_01 ( seed )
    write ( *, '(2x,i8,2x,f14.8)' ) i, r
  end do

  return
end
subroutine r4_normal_ab_test ( )

!*****************************************************************************80
!
!! R4_NORMAL_AB_TEST tests R4_NORMAL_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 4 ) mu
  real ( kind = 4 ) r
  real ( kind = 4 ) r4_normal_ab
  integer ( kind = 4 ) seed
  real ( kind = 4 ) sigma

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R4_NORMAL_AB_TEST'
  write ( *, '(a)' ) '  R4_NORMAL_AB computes real pseudonormal values '
  write ( *, '(a)' ) '  with mean MU and standard deviation SIGMA.'

  mu = 10.0E+00
  sigma = 2.0E+00
  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  MU = ', mu
  write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
  write ( *, '(a,i12)' ) '  SEED = ', seed
  write ( *, '(a)' ) ' '

  do i = 1, 10
    r = r4_normal_ab ( mu, sigma, seed )
    write ( *, '(2x,i8,2x,f14.8)' ) i, r
  end do

  return
end
subroutine r4_uniform_01_test ( )

!*****************************************************************************80
!
!! R4_UNIFORM_01_TEST tests R4_UNIFORM_01
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  real ( kind = 4 ) r
  real ( kind = 4 ) r4_uniform_01

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R4_UNIFORM_01_TEST'
  write ( *, '(a)' ) '  R4_UNIFORM_01 samples a uniform random'
  write ( *, '(a)' ) '  distribution in [0,1].'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  SEED = ', seed
  write ( *, '(a)' ) ''

  do i = 1, 10
    r = r4_uniform_01 ( seed )
    write ( *, '(2x,i2,2x,g14.6)' ) i, r
  end do

  return
end
subroutine r8_normal_01_test ( )

!*****************************************************************************80
!
!! R8_NORMAL_01_TEST tests R8_NORMAL_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_normal_01
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_NORMAL_01_TEST'
  write ( *, '(a)' ) '  R8_NORMAL_01 computes pseudonormal values '
  write ( *, '(a)' ) '  with mean 0.0 and standard deviation 1.0.'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  SEED = ', seed
  write ( *, '(a)' ) ' '

  do i = 1, 10
    r = r8_normal_01 ( seed )
    write ( *, '(2x,i8,2x,g14.6)' ) i, r
  end do

  return
end
subroutine r8_normal_ab_test ( )

!*****************************************************************************80
!
!! R8_NORMAL_AB_TEST tests R8_NORMAL_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_normal_ab
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_NORMAL_AB_TEST'
  write ( *, '(a)' ) '  R8_NORMAL_AB computes pseudonormal values '
  write ( *, '(a)' ) '  with mean MU and standard deviation SIGMA.'

  mu = 10.0D+00
  sigma = 2.0D+00
  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  MU = ', mu
  write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
  write ( *, '(a,i12)' ) '  SEED = ', seed
  write ( *, '(a)' ) ' '

  do i = 1, 10
    r = r8_normal_ab ( mu, sigma, seed )
    write ( *, '(2x,i8,2x,f14.8)' ) i, r
  end do

  return
end
subroutine r8_uniform_01_test ( )

!*****************************************************************************80
!
!! R8_UNIFORM_01_TEST tests R8_UNIFORM_01
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_UNIFORM_01_TEST'
  write ( *, '(a)' ) '  R8_UNIFORM_01 samples a uniform random'
  write ( *, '(a)' ) '  distribution in [0,1].'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  SEED = ', seed
  write ( *, '(a)' ) ''

  do i = 1, 10
    r = r8_uniform_01 ( seed )
    write ( *, '(2x,i2,2x,g14.6)' ) i, r
  end do

  return
end
subroutine r8mat_normal_01_test ( )

!*****************************************************************************80
!
!! R8MAT_NORMAL_01_TEST tests R8MAT_NORMAL_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) r(m,n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8MAT_NORMAL_01_TEST'
  write ( *, '(a)' ) '  R8MAT_NORMAL_01 returns a matrix of Normal 01 values.'

  seed = 123456789
  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  SEED = ', seed

  call r8mat_normal_01 ( m, n, seed, r )

  call r8mat_print ( m, n, r, '  Matrix of Normal 01 values:' )
  
  return
end
subroutine r8mat_normal_ab_test ( )

!*****************************************************************************80
!
!! R8MAT_NORMAL_AB_TEST tests R8MAT_NORMAL_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) mu
  real ( kind = 8 ) r(m,n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8MAT_NORMAL_AB_TEST'
  write ( *, '(a)' ) '  R8MAT_NORMAL_AB returns a matrix of Normal AB values.'

  mu = 100.0D+00
  sigma = 5.0D+00
  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  MU = ', mu
  write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
  write ( *, '(a,i12)' ) '  SEED = ', seed

  call r8mat_normal_ab ( m, n, mu, sigma, seed, r )

  call r8mat_print ( m, n, r, '  Matrix of Normal AB values:' )
  
  return
end
subroutine r8vec_normal_01_test ( )

!*****************************************************************************80
!
!! R8VEC_NORMAL_01_TEST tests R8VEC_NORMAL_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) r(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_NORMAL_01_TEST'
  write ( *, '(a)' ) '  R8VEC_NORMAL_01 computes a vector of Normal 01 values.'

  seed = 123456789
  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  SEED = ', seed

  call r8vec_normal_01 ( n, seed, r )

  call r8vec_print ( n, r, '  Vector of Normal 01 values:' )
  
  return
end
subroutine r8vec_normal_ab_test ( )

!*****************************************************************************80
!
!! R8VEC_NORMAL_AB_TEST tests R8VEC_NORMAL_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) mu
  real ( kind = 8 ) r(n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_NORMAL_AB_TEST'
  write ( *, '(a)' ) '  R8VEC_NORMAL_AB computes a vector of Normal AB values.'

  mu = 15.0D+00
  sigma = 0.25D+00
  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  MU = ', mu
  write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
  write ( *, '(a,i12)' ) '  SEED = ', seed

  call r8vec_normal_ab ( n, mu, sigma, seed, r )

  call r8vec_print ( n, r, '  Vector of Normal AB values:' )
  
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

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) r(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_UNIFORM_01_TEST'
  write ( *, '(a)' ) '  R8VEC_UNIFORM_01 returns a random R8VEC '
  write ( *, '(a)' ) '  with entries in [0,1].'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  SEED = ', seed

  call r8vec_uniform_01 ( n, seed, r )

  call r8vec_print ( n, r, '  Random R8VEC:' )

  return
end
