program main

!*****************************************************************************80
!
!! MAIN is the main program for I8LIB_TEST.
!
!  Discussion:
!
!    I8LIB_TEST tests the I8LIB library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I8LIB_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the I8LIB library.'

  call i8_choose_test ( )
  call i8_huge_test ( )
  call i8_huge_normalizer_test ( )
  call i8_uniform_ab_test ( )
  call i8_uniform2_test ( )
  call i8_uniform3_test ( )
  call i8_xor_test ( )
  call r8_uniform2_test ( )
  call r8_uniform3_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I8LIB_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine i8_choose_test ( )

!*****************************************************************************80
!
!! I8_CHOOSE_TEST tests I8_CHOOSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 8 ) cnk
  integer ( kind = 8 ) i8_choose
  integer ( kind = 8 ) k
  integer ( kind = 8 ) n

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I8_CHOOSE_TEST'
  write ( *, '(a)' ) '  I8_CHOOSE evaluates C(N,K).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       N       K      CNK'
  write ( *, '(a)' ) ' '

  do n = 0, 4
    do k = 0, n
      cnk = i8_choose ( n, k )
      write ( *, '(2x,i8,i8,i8)' ) n, k, cnk
    end do
  end do

  return
end
subroutine i8_huge_test ( )

!*****************************************************************************80
!
!! I8_HUGE_TEST tests I8_HUGE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 8 ) i8_huge
  integer ( kind = 8 ) dummy

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I8_HUGE_TEST'
  write ( *, '(a)' ) '  I8_HUGE returns a huge I8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i24)' ) '  I8_HUGE() = ', i8_huge ( )
  write ( *, '(a,i24)' ) '  HUGE(1) =   ', huge ( dummy )

  return
end
subroutine i8_huge_normalizer_test ( )

!*****************************************************************************80
!
!! I8_HUGE_NORMALIZER_TEST tests I8_HUGE_NORMALIZER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 8 ) i8
  integer ( kind = 8 ) i8_huge
  real ( kind = 8 ) i8_huge_normalizer
  real ( kind = 8 ) r8
  real ( kind = 8 ) value

  i8 = i8_huge ( )
  r8 = i8_huge_normalizer ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I8_HUGE_NORMALIZER_TEST'
  write ( *, '(a)' ) '  I8_HUGE_NORMALIZER returns 1/(I8_HUGE+1).'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i24)' ) '  I8_HUGE() = ', i8
  write ( *, '(a,g14.6)' ) '  I8_HUGE_NORMALIZER() = ', r8

  value = real ( i8, kind = 8 ) * r8

  write ( *, '(a)' ) ' '
  write ( *, '(a,g24.16)' ) '      real ( I8_HUGE ) * I8_HUGE_NORMALIZER = ', value
  write ( *, '(a,g24.16)' ) '  1 - real ( I8_HUGE ) * I8_HUGE_NORMALIZER = ', 1.0D+00 - value


  return
end
subroutine i8_uniform_ab_test ( )

!*****************************************************************************80
!
!! I8_UNIFORM_AB_TEST tests I8_UNIFORM_AB
!
!  Discussion:
!
!    This test should demonstrate that I8_UNIFORM_AB samples EVERY
!    integer between A and B, and with reasonable evenness.
!
!    The algorithm is not perfect, and especially for values of A and B
!    close to the largest integer, some bias may be exhibited.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 8 ) a
  integer ( kind = 8 ) b
  integer ( kind = 8 ) i
  integer ( kind = 8 ) i8_uniform_ab
  integer ( kind = 8 ) seed
  integer ( kind = 8 ) value

  a = 1000000000000000000_8
  b = 1000000000000000009_8
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I8_UNIFORM_AB_TEST'
  write ( *, '(a)' ) '  I8_UNIFORM_AB samples a uniform random'
  write ( *, '(a)' ) '  integer distribution in [A,B].'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i24)' ) '  A =   ', a
  write ( *, '(a,i24)' ) '  B =   ', b
  write ( *, '(a)' ) ' '

  do i = 1, 20
    value = i8_uniform_ab ( a, b, seed )
    write ( *, '(2x,i4,2x,i24)' ) i, value
  end do

  return
end
subroutine i8_uniform2_test ( )

!*****************************************************************************80
!
!! I8_UNIFORM2_TEST tests I8_UNIFORM2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 8 ) i
  integer ( kind = 8 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I8_UNIFORM2_TEST'
  write ( *, '(a)' ) '  I8_UNIFORM2 samples a uniform random'
  write ( *, '(a)' ) '  integer distribution in [-2^63,2^63-1].'
  write ( *, '(a)' ) ' '

  i = 0
  seed = 123456789
  write ( *, '(2x,i4,2x,i24)' ) i, seed

  do i = 1, 10
    call i8_uniform2 ( seed )
    write ( *, '(2x,i4,2x,i24)' ) i, seed
  end do

  return
end
subroutine i8_uniform3_test ( )

!*****************************************************************************80
!
!! I8_UNIFORM3_TEST tests I8_UNIFORM3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 8 ) i
  integer ( kind = 8 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I8_UNIFORM3_TEST'
  write ( *, '(a)' ) '  I8_UNIFORM3 samples a uniform random'
  write ( *, '(a)' ) '  integer distribution in [0,2**63-1].'
  write ( *, '(a)' ) ' '

  i = 0
  seed = 123456789
  write ( *, '(2x,i4,2x,i24)' ) i, seed
  do i = 1, 10
    call i8_uniform3 ( seed )
    write ( *, '(2x,i4,2x,i24)' ) i, seed
  end do

  return
end
subroutine i8_xor_test ( )

!*****************************************************************************80
!
!! I8_XOR_TEST tests I8_XOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 8 ) i
  integer ( kind = 8 ) :: ihi = 100
  integer ( kind = 8 ) :: ilo = 0
  integer ( kind = 8 ) i8_uniform_ab
  integer ( kind = 8 ) i8_xor
  integer ( kind = 8 ) j
  integer ( kind = 8 ) k
  integer ( kind = 8 ) l
  integer ( kind = 8 ) seed
  integer ( kind = 8 ) test
  integer ( kind = 8 ), parameter :: test_num = 10

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I8_XOR_TEST'
  write ( *, '(a)' ) '  I8_XOR returns the bitwise exclusive OR of'
  write ( *, '(a)' ) '  two I8''s.'
  write ( *, '(a)' ) '  Compare the FORTRAN intrinsic IEOR.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         I         J    I8_XOR      IEOR'
  write ( *, '(a)' ) ' '

  do test = 1, test_num
    i = i8_uniform_ab ( ilo, ihi, seed )
    j = i8_uniform_ab ( ilo, ihi, seed )
    k = i8_xor ( i, j )
    l = ieor ( i, j )
    write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) i, j, k, l
  end do

  return
end
subroutine r8_uniform2_test ( )

!*****************************************************************************80
!
!! R8_UNIFORM2_TEST tests R8_UNIFORM2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 8 ) i
  real ( kind = 8 ) r8
  real ( kind = 8 ) r8_uniform2
  integer ( kind = 8 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_UNIFORM2_TEST'
  write ( *, '(a)' ) '  R8_UNIFORM2 samples a uniform random'
  write ( *, '(a)' ) '  integer distribution in [-1,+1].'
  write ( *, '(a)' ) ' '

  do i = 1, 50
    r8 = r8_uniform2 ( seed )
    write ( *, '(2x,i4,2x,g24.16)' ) i, r8
  end do

  return
end
subroutine r8_uniform3_test ( )

!*****************************************************************************80
!
!! R8_UNIFORM3_TEST tests R8_UNIFORM3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 8 ) i
  real ( kind = 8 ) r8
  real ( kind = 8 ) r8_uniform3
  integer ( kind = 8 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_UNIFORM3_TEST'
  write ( *, '(a)' ) '  R8_UNIFORM3 samples a uniform random'
  write ( *, '(a)' ) '  integer distribution in [0,+1].'
  write ( *, '(a)' ) ' '

  do i = 1, 50
    r8 = r8_uniform3 ( seed )
    write ( *, '(2x,i4,2x,g24.16)' ) i, r8
  end do

  return
end
