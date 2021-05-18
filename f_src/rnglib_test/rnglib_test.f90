program main

!*****************************************************************************80
!
!! MAIN is the main program for RNGLIB_TEST.
!
!  Discussion:
!
!    RNGLIB_TEST tests the RNGLIB library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 January 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'RNGLIB_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the RNGLIB library.'
!
!  Initialize RNGLIB.
!
  call initialize ( )
!
!  Function tests.
!
  call i4_uni_test ( )
  call r8_uni_01_test ( )
!
!  Feature tests.
!
  call seed_reset_test ( )
  call multiple_stream_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'RNGLIB_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  return
end
subroutine i4_uni_test ( )

!*****************************************************************************80
!
!! I4_UNI_TEST tests I4_UNI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uni
  integer ( kind = 4 ) value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_UNI_TEST'
  write ( *, '(a)' ) '  I4_UNI returns a random positive integer.'
  write ( *, '(a)' ) ''

  do i = 1, 20
    value = i4_uni ( )
    write ( *, '(2x,i2,2x,i12)' ) i, value
  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_UNI_TEST:'
  write ( *, '(a)' )'  Normal end of execution.'

  return
end
subroutine r8_uni_01_test ( )

!*****************************************************************************80
!
!! R8_UNI_01_TEST tests R8_UNI_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_uni_01
  real ( kind = 8 ) value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_UNI_01_TEST'
  write ( *, '(a)' ) '  R8_UNI_01 returns a random real in [0,1].'
  write ( *, '(a)' ) ''

  do i = 1, 20
    value = r8_uni_01 ( )
    write ( *, '(2x,i2,2x,g14.6)' ) i, value
  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_UNI_01_TEST:'
  write ( *, '(a)' )'  Normal end of execution.'

  return
end
subroutine seed_reset_test ( )

!*****************************************************************************80
!
!! SEED_RESET_TEST demonstrates how the seed can be reset to its initial or last value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 August 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  real ( kind = 4 ) r4_uni_01
  real ( kind = 4 ) u

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SEED_RESET_TEST'
  write ( *, '(a)' ) '  R4_UNI_01 ( ) returns a random real number'
  write ( *, '(a)' ) '  in [0,1] using the current generator.'
!
!  Initialize the package.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  INITIALIZE initializes the random number generator.'
  write ( *, '(a)' ) '  It only needs to be called once before using the package.'

  call initialize ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  INIT_GENERATOR can reset the seed to the initial value,'
  write ( *, '(a)' ) '  the last (previous) value, or a new seed.'
!
!  Set the current generator index to 17.
!
  g = 17
  call cgn_set ( g )
  write ( *, '(a)' ) ' '
  write ( *, '(a,i2)' ) '  Current generator index = ', g
!
!  Force the current generator to begin at its initial seed.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  INIT_GENERATOR ( 0 ) starts at the initial seed.'

  call init_generator ( 0 )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   I    R4_UNI_01 ( )'
  write ( *, '(a)' ) ' '
  do i = 1, 10
    u = r4_uni_01 ( )
    write ( *, '(2x,i2,2x,g14.6)' ) i, u
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Calling INIT_GENERATOR ( 0 ) again restarts'
  write ( *, '(a)' ) '  at the initial seed.'

  call init_generator ( 0 )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   I    R4_UNI_01 ( )'
  write ( *, '(a)' ) ' '
  do i = 1, 10
    u = r4_uni_01 ( )
    write ( *, '(2x,i2,2x,g14.6)' ) i, u
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Calling INIT_GENERATOR ( 2 ) restarts'
  write ( *, '(a)' ) '  at a new "far ahead" seed.'

  call init_generator ( 2 )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   I    R4_UNI_01 ( )'
  write ( *, '(a)' ) ' '
  do i = 1, 10
    u = r4_uni_01 ( )
    write ( *, '(2x,i2,2x,g14.6)' ) i, u
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Calling INIT_GENERATOR ( 1 ) restarts'
  write ( *, '(a)' ) '  at the last seed (in this case, the "far ahead"'
  write ( *, '(a)' ) '  seed specified on the previous call.)'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   I    R4_UNI_01 ( )'
  write ( *, '(a)' ) ' '
  do i = 1, 10
    u = r4_uni_01 ( )
    write ( *, '(2x,i2,2x,g14.6)' ) i, u
    if ( mod ( i, 3 ) == 0 ) then
      call init_generator ( 1 )
      write ( *, '(a)' ) '  (Reset to last seed)'
    end if
  end do

  return
end
subroutine multiple_stream_test ( )

!*****************************************************************************80
!
!! MULTIPLE_STREAM_TEST demonstrates the use of multiple streams.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 August 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) g(3)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 4 ) r4_uni_01
  real ( kind = 4 ) u(3)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MULTIPLE_STREAM_TEST'
  write ( *, '(a)' ) '  R4_UNI_01 ( ) returns a random real number'
  write ( *, '(a)' ) '  in [0,1] using the current generator.'
!
!  Initialize the package.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  INITIALIZE initializes the random number generator.'
  write ( *, '(a)' ) '  It only needs to be called once before using the package.'

  call initialize ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Let us call generators #3, #6 and #9.'
!
!  Use three separate generators, 3, 6 and 9.
!  Force them to start at their initial seeds.
!
  g(1) = 3
  g(2) = 6
  g(3) = 9
  write ( *, '(a)' ) ' '
  do i = 1, 3
    write ( *, '(a,i1)' ) '  Initialize generator ', g(i)
    call cgn_set ( g(i) )
    call init_generator ( 0 )
  end do
!
!  Call the generators in the order 3, 6, 9.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   I    R4_UNI_01 ( 3 )  R4_UNI_01 ( 6 )  R4_UNI_01 ( 9 )'
  write ( *, '(a)' ) ' '
  do i = 1, 10
    do j = 1, 3
      call cgn_set ( g(j) )
      u(j) = r4_uni_01 ( )
    end do
    write ( *, '(2x,i2,2x,g14.6,4x,g14.6,4x,g14.6)' ) i, u(1:3)
  end do
!
!  Restart the generators at their initial seeds.
!
  g(1) = 6
  g(2) = 9
  g(3) = 3

  write ( *, '(a)' ) ' '
  do i = 1, 3
    write ( *, '(a,i1)' ) '  Reinitialize generator ', g(i)
    call cgn_set ( g(i) )
    call init_generator ( 0 )
  end do
!
!  Call them in a different order, same result.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   I    R4_UNI_01 ( 6 )  R4_UNI_01 ( 9 )  R4_UNI_01 ( 3 )'
  write ( *, '(a)' ) ' '
  do i = 1, 10
    do j = 1, 3
      call cgn_set ( g(j) )
      u(j) = r4_uni_01 ( )
    end do
    write ( *, '(2x,i2,2x,g14.6,4x,g14.6,4x,g14.6)' ) i, u(1:3)
  end do

  return
end
