program main

!*****************************************************************************80
!
!! MAIN is the main program for RANLIB_TEST.
!
!  Discussion:
!
!    RANLIB_TEST tests the RANLIB library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = 100 ) phrase

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'RANLIB_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the RANLIB library.'

  phrase = 'randomizer'

  call test_phrtsd ( phrase )

  call test_bot ( )

  call test_genbet ( phrase )
  call test_ignbin ( phrase )
  call test_genchi ( phrase )
  call test_genexp ( phrase )
  call test_r4_exponential_sample ( phrase )
  call test_genf ( phrase )
  call test_gengam ( phrase )
  call test_ignnbn ( phrase )
  call test_gennch ( phrase )
  call test_gennf ( phrase )
  call test_gennor ( phrase )
  call test_ignpoi ( phrase )
  call test_genunf ( phrase )

  call snorm_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'RANLIB_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test_phrtsd ( phrase )

!*****************************************************************************80
!
!! TEST_PHRTSD tests PHRTSD, which produces two seeds from a random phrase.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = * ) phrase
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_PHRTSD'
  write ( *, '(a)' ) '  Test PHRTSD,'
  write ( *, '(a)' ) '  which generates two seeds from a phrase.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The input phrase is "' // trim ( phrase ) // '".'

  call phrtsd ( phrase, seed1, seed2 )
  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  SEED1 = ', seed1
  write ( *, '(a,i12)' ) '  SEED2 = ', seed2

  return
end
subroutine test_bot ( )

!*****************************************************************************80
!
!! TEST_BOT is a test program for the bottom level routines
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) answer(10000)
  integer ( kind = 4 ) genlst(5)
  integer ( kind = 4 ) i4_uni
  integer ( kind = 4 ) ians
  integer ( kind = 4 ) iblock
  integer ( kind = 4 ) igen
  integer ( kind = 4 ) itmp
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) ixgen
  integer ( kind = 4 ) nbad
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2

  save genlst

  data genlst / 1, 5, 10, 20, 32 /

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_BOT'
  write ( *, '(a)' ) '  Test the lower level random number generators.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Five of the 32 generators will be tested.'
  write ( *, '(a)' ) '  We generate 100000 numbers, reset the block'
  write ( *, '(a)' ) '  and do it again.  No disagreements should occur.'
  write ( *, '(a)' ) ' '
!
!  Initialize the generators.
!
  call initialize ( )
!
!  Set up all generators.
!
  seed1 = 12345
  seed2 = 54321
  call set_initial_seed ( seed1, seed2 )
!
!  For a selected set of generators
!
  nbad = 0

  do ixgen = 1, 5

    igen = genlst(ixgen)
    call cgn_set ( igen )
    write ( *, '(a,i2)' ) '  Testing generator ', igen
!
!  Use 10 blocks, and generate 1000 numbers per block
!
    call init_generator ( 0 )

    do iblock = 1, 10
      do ians = 1, 1000
        ix = ians + ( iblock - 1 ) * 1000
        answer(ix) = i4_uni ( )
      end do
      call init_generator ( 2 )
    end do
!
!  Do it again and compare answers
!
    call init_generator ( 0 )

    do iblock = 1, 10
      do ians = 1, 1000
        ix = ians + ( iblock - 1 ) * 1000
        itmp = i4_uni ( )

        if ( itmp /= answer(ix) ) then

          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'TEST_BOT - Warning!'
          write ( *, '(a)' ) '  Data disagreement:'
          write ( *, '(a,i6)' ) '  Block = ', iblock
          write ( *, '(a,i6)' ) '  N within block = ', ians
          write ( *, '(a,i6)' ) '  Index in ANSWER = ', ix
          write ( *, '(a,i10)' ) '  First value =  ', answer(ix)
          write ( *, '(a,i10)' ) '  Second value = ', itmp

          nbad = nbad + 1

          if ( 10 .lt. nbad ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'TEST_BOT - Warning!'
            write ( *, '(a)' ) '  More than 10 mismatches!'
            write ( *, '(a)' ) '  Tests terminated early.'
            return
          end if

        end if

      end do

      call init_generator ( 2 )

    end do

  end do

  return
end
subroutine test_genbet ( phrase )

!*****************************************************************************80
!
!! TEST_GENBET tests GENBET, which generates Beta deviates.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  parameter ( n = 1000 )

  real ( kind = 4 ) a
  real ( kind = 4 ) array(n)
  real ( kind = 4 ) av
  real ( kind = 4 ) avtr
  real ( kind = 4 ) b
  real ( kind = 4 ) genbet
  real ( kind = 4 ) genunf
  real ( kind = 4 ) high
  integer ( kind = 4 ) i
  real ( kind = 4 ) low
  real ( kind = 4 ) param(2)
  character ( len = 4 ) pdf
  character ( len = * ) phrase
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2
  real ( kind = 4 ) var
  real ( kind = 4 ) vartr
  real ( kind = 4 ) xmax
  real ( kind = 4 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_GENBET'
  write ( *, '(a)' ) '  GENBET generates Beta deviates.'
!
!  Initialize the generators.
!
  call initialize ( )
!
!  Set the seeds based on the phrase.
!
  call phrtsd ( phrase, seed1, seed2 )
!
!  Initialize all generators.
!
  call set_initial_seed ( seed1, seed2 )
!
!  Select the parameters at random within a given range.
!
  low = 1.0E+00
  high = 10.0E+00
  a = genunf ( low, high )

  low = 1.0E+00
  high = 10.0E+00
  b = genunf ( low, high )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  N = ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Parameters:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  A = ', a
  write ( *, '(a,g14.6)' ) '  B = ', b
!
!  Generate N samples.
!
  do i = 1, n
    array(i) = genbet ( a, b )
  end do
!
!  Compute statistics on the samples.
!
  call stats ( array, n, av, var, xmin, xmax )
!
!  Request expected value of statistics for this distribution.
!
  pdf = 'bet'
  param(1) = a
  param(2) = b
  call trstat ( pdf, param, avtr, vartr )

  write ( *, '(a)' ) ' '
  write ( *, '(a,2g14.6)' ) '  Sample data range:          ', xmin, xmax
  write ( *, '(a,2g14.6)' ) '  Sample mean, variance:      ', av,   var
  write ( *, '(a,2g14.6)' ) '  Distribution mean, variance ', avtr, vartr

  return
end
subroutine test_ignbin ( phrase )

!*****************************************************************************80
!
!! TEST_IGNBIN tests IGNBIN, which generates Binomial deviates.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  parameter ( n = 10000 )

  real ( kind = 4 ) array(n)
  real ( kind = 4 ) av
  real ( kind = 4 ) avtr
  real ( kind = 4 ) genunf
  real ( kind = 4 ) high
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ignbin
  real ( kind = 4 ) low
  integer ( kind = 4 ) nn
  real ( kind = 4 ) param(2)
  character ( len = 4 ) pdf
  character ( len = * ) phrase
  real ( kind = 4 ) pp
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2
  real ( kind = 4 ) var
  real ( kind = 4 ) vartr
  real ( kind = 4 ) xmax
  real ( kind = 4 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_IGNBIN'
  write ( *, '(a)' ) '  IGNBIN generates binomial deviates.'
!
!  Initialize the generators.
!
  call initialize ( )
!
!  Set the seeds based on the phrase.
!
  call phrtsd ( phrase, seed1, seed2 )
!
!  Initialize all generators.
!
  call set_initial_seed ( seed1, seed2 )
!
!  Select the parameters at random within a given range.
!
  low = 0.5E+00
  high = 20.0E+00
  nn = int ( genunf ( low, high ) )

  low = 0.0E+00
  high = 1.0E+00
  pp = genunf ( low, high )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  N = ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Parameters:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  NN = ', nn
  write ( *, '(a,g14.6)' ) '  PP = ', pp
!
!  Generate N samples.
!
  do i = 1, n
    array(i) = real ( ignbin ( nn, pp ) )
  end do
!
!  Compute statistics on the samples.
!
  call stats ( array, n, av, var, xmin, xmax )
!
!  Request expected value of statistics for this distribution.
!
  pdf = 'bin'
  param(1) = real ( nn )
  param(2) = pp
  call trstat ( pdf, param, avtr, vartr )

  write ( *, '(a)' ) ' '
  write ( *, '(a,2g14.6)' ) '  Sample data range:          ', xmin, xmax
  write ( *, '(a,2g14.6)' ) '  Sample mean, variance:      ', av,   var
  write ( *, '(a,2g14.6)' ) '  Distribution mean, variance ', avtr, vartr

  return
end
subroutine test_genchi ( phrase )

!*****************************************************************************80
!
!! TEST_GENCHI tests GENCHI, which generates Chi-Square deviates.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  parameter ( n = 1000 )

  real ( kind = 4 ) array(n)
  real ( kind = 4 ) av
  real ( kind = 4 ) avtr
  real ( kind = 4 ) df
  real ( kind = 4 ) genchi
  real ( kind = 4 ) genunf
  real ( kind = 4 ) high
  integer ( kind = 4 ) i
  real ( kind = 4 ) low
  real ( kind = 4 ) param(1)
  character ( len = 4 ) pdf
  character ( len = * ) phrase
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2
  real ( kind = 4 ) var
  real ( kind = 4 ) vartr
  real ( kind = 4 ) xmax
  real ( kind = 4 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_GENCHI'
  write ( *, '(a)' ) '  GENCHI generates Chi-square deviates.'
!
!  Initialize the generators.
!
  call initialize ( )
!
!  Set the seeds based on the phrase.
!
  call phrtsd ( phrase, seed1, seed2 )
!
!  Initialize all generators.
!
  call set_initial_seed ( seed1, seed2 )
!
!  Select the parameters at random within a given range.
!
  low = 1.0E+00
  high = 10.0E+00
  df = genunf ( low, high )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  N = ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Parameters:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  DF = ', df
!
!  Generate N samples.
!
  do i = 1, n
    array(i) = genchi ( df )
  end do
!
!  Compute statistics on the samples.
!
  call stats ( array, n, av, var, xmin, xmax )
!
!  Request expected value of statistics for this distribution.
!
  pdf = 'chi'
  param(1) = df
  call trstat ( pdf, param, avtr, vartr )

  write ( *, '(a)' ) ' '
  write ( *, '(a,2g14.6)' ) '  Sample data range:          ', xmin, xmax
  write ( *, '(a,2g14.6)' ) '  Sample mean, variance:      ', av,   var
  write ( *, '(a,2g14.6)' ) '  Distribution mean, variance ', avtr, vartr

  return
end
subroutine test_genexp ( phrase )

!*****************************************************************************80
!
!! TEST_GENEXP tests GENEXP, which generates exponential deviates.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  parameter ( n = 1000 )

  real ( kind = 4 ) array(n)
  real ( kind = 4 ) av
  real ( kind = 4 ) avtr
  real ( kind = 4 ) genexp
  real ( kind = 4 ) genunf
  real ( kind = 4 ) high
  integer ( kind = 4 ) i
  real ( kind = 4 ) low
  real ( kind = 4 ) mu
  real ( kind = 4 ) param(2)
  character ( len = 4 ) pdf
  character ( len = * ) phrase
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2
  real ( kind = 4 ) var
  real ( kind = 4 ) vartr
  real ( kind = 4 ) xmax
  real ( kind = 4 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_GENEXP'
  write ( *, '(a)' ) '  GENEXP generates exponential deviates.'
!
!  Initialize the generators.
!
  call initialize ( )
!
!  Set the seeds based on the phrase.
!
  call phrtsd ( phrase, seed1, seed2 )
!
!  Initialize all generators.
!
  call set_initial_seed ( seed1, seed2 )
!
!  Select the parameters at random within a given range.
!
  low =  0.5E+00
  high = 10.0E+00
  mu = genunf ( low, high )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  N = ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Parameters:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  MU =   ', mu
!
!  Generate N samples.
!
  do i = 1, n
    array(i) = genexp ( mu )
  end do
!
!  Compute statistics on the samples.
!
  call stats ( array, n, av, var, xmin, xmax )
!
!  Request expected value of statistics for this distribution.
!
  pdf = 'exp'
  param(1) = mu

  call trstat ( pdf, param, avtr, vartr )

  write ( *, '(a)' ) ' '
  write ( *, '(a,2g14.6)' ) '  Sample data range:          ', xmin, xmax
  write ( *, '(a,2g14.6)' ) '  Sample mean, variance:      ', av,   var
  write ( *, '(a,2g14.6)' ) '  Distribution mean, variance ', avtr, vartr

  return
end
subroutine test_r4_exponential_sample ( phrase )

!*****************************************************************************80
!
!! TEST_R4_EXPONENTIAL_SAMPLE tests a function for exponential deviates.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  parameter ( n = 1000 )

  real ( kind = 4 ) array(n)
  real ( kind = 4 ) av
  real ( kind = 4 ) avtr
  real ( kind = 4 ) genunf
  real ( kind = 4 ) high
  integer ( kind = 4 ) i
  real ( kind = 4 ) low
  real ( kind = 4 ) mu
  real ( kind = 4 ) param(2)
  character ( len = 4 ) pdf
  character ( len = * ) phrase
  real (  kind = 4 ) r4_exponential_sample
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2
  real ( kind = 4 ) var
  real ( kind = 4 ) vartr
  real ( kind = 4 ) xmax
  real ( kind = 4 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_R4_EXPONENTIAL_SAMPLE'
  write ( *, '(a)' ) '  R4_EXPONENTIAL_SAMPLE generates exponential deviates.'
!
!  Initialize the generators.
!
  call initialize ( )
!
!  Set the seeds based on the phrase.
!
  call phrtsd ( phrase, seed1, seed2 )
!
!  Initialize all generators.
!
  call set_initial_seed ( seed1, seed2 )
!
!  Select the parameters at random within a given range.
!
  low =  0.5E+00
  high = 10.0E+00
  mu = genunf ( low, high )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  N = ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Parameters:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  MU =   ', mu
!
!  Generate N samples.
!
  do i = 1, n
    array(i) = r4_exponential_sample ( mu )
  end do
!
!  Compute statistics on the samples.
!
  call stats ( array, n, av, var, xmin, xmax )
!
!  Request expected value of statistics for this distribution.
!
  pdf = 'exp'
  param(1) = mu

  call trstat ( pdf, param, avtr, vartr )

  write ( *, '(a)' ) ' '
  write ( *, '(a,2g14.6)' ) '  Sample data range:          ', xmin, xmax
  write ( *, '(a,2g14.6)' ) '  Sample mean, variance:      ', av,   var
  write ( *, '(a,2g14.6)' ) '  Distribution mean, variance ', avtr, vartr

  return
end
subroutine test_genf ( phrase )

!*****************************************************************************80
!
!! TEST_GENF tests GENF, which generates F deviates.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  parameter ( n = 10000 )

  real ( kind = 4 ) array(n)
  real ( kind = 4 ) av
  real ( kind = 4 ) avtr
  real ( kind = 4 ) dfd
  real ( kind = 4 ) dfn
  real ( kind = 4 ) genf
  real ( kind = 4 ) genunf
  real ( kind = 4 ) high
  integer ( kind = 4 ) i
  real ( kind = 4 ) low
  real ( kind = 4 ) param(2)
  character ( len = 4 ) pdf
  character ( len = * ) phrase
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2
  real ( kind = 4 ) var
  real ( kind = 4 ) vartr
  real ( kind = 4 ) xmax
  real ( kind = 4 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_GENF'
  write ( *, '(a)' ) '  GENF generates F deviates.'
!
!  Initialize the generators.
!
  call initialize ( )
!
!  Set the seeds based on the phrase.
!
  call phrtsd ( phrase, seed1, seed2 )
!
!  Initialize all generators.
!
  call set_initial_seed ( seed1, seed2 )
!
!  Select the parameters at random within a given range.
!
  low = 3.0E+00
  high = 10.0E+00
  dfn = genunf ( low, high )

  low = 5.0E+00
  high = 10.0E+00
  dfd = genunf ( low, high )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  N = ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Parameters:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  DFN =   ', dfn
  write ( *, '(a,g14.6)' ) '  DFD =   ', dfd
!
!  Generate N samples.
!
  do i = 1, n
    array(i) = genf ( dfn, dfd )
  end do
!
!  Compute statistics on the samples.
!
  call stats ( array, n, av, var, xmin, xmax )
!
!  Request expected value of statistics for this distribution.
!
  pdf = 'f'
  param(1) = dfn
  param(2) = dfd

  call trstat ( pdf, param, avtr, vartr )

  write ( *, '(a)' ) ' '
  write ( *, '(a,2g14.6)' ) '  Sample data range:          ', xmin, xmax
  write ( *, '(a,2g14.6)' ) '  Sample mean, variance:      ', av,   var
  write ( *, '(a,2g14.6)' ) '  Distribution mean, variance ', avtr, vartr

  return
end
subroutine test_gengam ( phrase )

!*****************************************************************************80
!
!! TEST_GENGAM tests GENGAM, which generates Gamma deviates.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  parameter ( n = 1000 )

  real ( kind = 4 ) a
  real ( kind = 4 ) array(n)
  real ( kind = 4 ) av
  real ( kind = 4 ) avtr
  real ( kind = 4 ) gengam
  real ( kind = 4 ) genunf
  real ( kind = 4 ) high
  integer ( kind = 4 ) i
  real ( kind = 4 ) low
  real ( kind = 4 ) param(2)
  character ( len = 4 ) pdf
  character ( len = * ) phrase
  real ( kind = 4 ) r
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2
  real ( kind = 4 ) var
  real ( kind = 4 ) vartr
  real ( kind = 4 ) xmax
  real ( kind = 4 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_GENGAM'
  write ( *, '(a)' ) '  GENGAM generates Gamma deviates.'
!
!  Initialize the generators.
!
  call initialize ( )
!
!  Set the seeds based on the phrase.
!
  call phrtsd ( phrase, seed1, seed2 )
!
!  Initialize all generators.
!
  call set_initial_seed ( seed1, seed2 )
!
!  Select the parameters at random within a given range.
!
  low = 1.0E+00
  high = 10.0E+00
  a = genunf ( low, high )

  low = 1.0E+00
  high = 10.0E+00
  r = genunf ( low, high )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  N = ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Parameters:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  A = ', a
  write ( *, '(a,g14.6)' ) '  R = ', r
!
!  Generate N samples.
!
  do i = 1, n
    array(i) = gengam ( a, r )
  end do
!
!  Compute statistics on the samples.
!
  call stats ( array, n, av, var, xmin, xmax )
!
!  Request expected value of statistics for this distribution.
!
  pdf = 'gam'
  param(1) = a
  param(2) = r
  call trstat ( pdf, param, avtr, vartr )

  write ( *, '(a)' ) ' '
  write ( *, '(a,2g14.6)' ) '  Sample data range:          ', xmin, xmax
  write ( *, '(a,2g14.6)' ) '  Sample mean, variance:      ', av,   var
  write ( *, '(a,2g14.6)' ) '  Distribution mean, variance ', avtr, vartr

  return
end
subroutine test_ignnbn ( phrase )

!*****************************************************************************80
!
!! TEST_IGNNBN tests IGNNBN, which generates Negative Binomial deviates.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  parameter ( n = 10000 )

  real ( kind = 4 ) array(n)
  real ( kind = 4 ) av
  real ( kind = 4 ) avtr
  real ( kind = 4 ) genunf
  real ( kind = 4 ) high
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ignnbn
  real ( kind = 4 ) low
  integer ( kind = 4 ) nn
  real ( kind = 4 ) param(2)
  character ( len = 4 ) pdf
  character ( len = * ) phrase
  real ( kind = 4 ) pp
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2
  real ( kind = 4 ) var
  real ( kind = 4 ) vartr
  real ( kind = 4 ) xmax
  real ( kind = 4 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_IGNNBN'
  write ( *, '(a)' ) '  IGNNBN generates negative binomial deviates.'
!
!  Initialize the generators.
!
  call initialize ( )
!
!  Set the seeds based on the phrase.
!
  call phrtsd ( phrase, seed1, seed2 )
!
!  Initialize all generators.
!
  call set_initial_seed ( seed1, seed2 )
!
!  Select the parameters at random within a given range.
!
  low = 3.0E+00
  high = 20.0E+00
  nn = int ( genunf ( low, high ) )

  low = 0.0E+00
  high = 1.0E+00
  pp = genunf ( low, high )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  N = ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Parameters:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  NN = ', nn
  write ( *, '(a,g14.6)' ) '  PP = ', pp
!
!  Generate N samples.
!
  do i = 1, n
    array(i) = real ( ignnbn ( nn, pp ) )
  end do
!
!  Compute statistics on the samples.
!
  call stats ( array, n, av, var, xmin, xmax )
!
!  Request expected value of statistics for this distribution.
!
  pdf = 'nbn'
  param(1) = real ( nn )
  param(2) = pp
  call trstat ( pdf, param, avtr, vartr )

  write ( *, '(a)' ) ' '
  write ( *, '(a,2g14.6)' ) '  Sample data range:          ', xmin, xmax
  write ( *, '(a,2g14.6)' ) '  Sample mean, variance:      ', av,   var
  write ( *, '(a,2g14.6)' ) '  Distribution mean, variance ', avtr, vartr

  return
end
subroutine test_gennch ( phrase )

!*****************************************************************************80
!
!! TEST_GENNCH tests GENNCH, which generates noncentral Chi-Square deviates.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  parameter ( n = 1000 )

  real ( kind = 4 ) array(n)
  real ( kind = 4 ) av
  real ( kind = 4 ) avtr
  real ( kind = 4 ) df
  real ( kind = 4 ) gennch
  real ( kind = 4 ) genunf
  real ( kind = 4 ) high
  integer ( kind = 4 ) i
  real ( kind = 4 ) low
  real ( kind = 4 ) param(2)
  character ( len = 4 ) pdf
  character ( len = * ) phrase
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2
  real ( kind = 4 ) var
  real ( kind = 4 ) vartr
  real ( kind = 4 ) xmax
  real ( kind = 4 ) xmin
  real ( kind = 4 ) xnonc

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_GENNCH'
  write ( *, '(a)' ) '  GENNCH generates noncentral Chi-square deviates.'
!
!  Initialize the generators.
!
  call initialize ( )
!
!  Set the seeds based on the phrase.
!
  call phrtsd ( phrase, seed1, seed2 )
!
!  Initialize all generators.
!
  call set_initial_seed ( seed1, seed2 )
!
!  Select the parameters at random within a given range.
!
  low = 2.0E+00
  high = 10.0E+00
  df = genunf ( low, high )

  low = 0.0E+00
  high = 2.0E+00
  xnonc = genunf ( low, high )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  N = ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Parameters:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  DF =    ', df
  write ( *, '(a,g14.6)' ) '  XNONC = ', xnonc
!
!  Generate N samples.
!
  do i = 1, n
    array(i) = gennch ( df, xnonc )
  end do
!
!  Compute statistics on the samples.
!
  call stats ( array, n, av, var, xmin, xmax )
!
!  Request expected value of statistics for this distribution.
!
  pdf = 'nch'
  param(1) = df
  param(2) = xnonc
  call trstat ( pdf, param, avtr, vartr )

  write ( *, '(a)' ) ' '
  write ( *, '(a,2g14.6)' ) '  Sample data range:          ', xmin, xmax
  write ( *, '(a,2g14.6)' ) '  Sample mean, variance:      ', av,   var
  write ( *, '(a,2g14.6)' ) '  Distribution mean, variance ', avtr, vartr

  return
end
subroutine test_gennf ( phrase )

!*****************************************************************************80
!
!! TEST_GENNF tests GENNF, which generates noncentral F deviates.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  parameter ( n = 10000 )

  real ( kind = 4 ) array(n)
  real ( kind = 4 ) av
  real ( kind = 4 ) avtr
  real ( kind = 4 ) dfd
  real ( kind = 4 ) dfn
  real ( kind = 4 ) gennf
  real ( kind = 4 ) genunf
  real ( kind = 4 ) high
  integer ( kind = 4 ) i
  real ( kind = 4 ) low
  real ( kind = 4 ) param(3)
  character ( len = 4 ) pdf
  character ( len = * ) phrase
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2
  real ( kind = 4 ) var
  real ( kind = 4 ) vartr
  real ( kind = 4 ) xmax
  real ( kind = 4 ) xmin
  real ( kind = 4 ) xnonc

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_GENNF'
  write ( *, '(a)' ) '  GENNF generates noncentral F deviates.'
!
!  Initialize the generators.
!
  call initialize ( )
!
!  Set the seeds based on the phrase.
!
  call phrtsd ( phrase, seed1, seed2 )
!
!  Initialize all generators.
!
  call set_initial_seed ( seed1, seed2 )
!
!  Select the parameters at random within a given range.
!
  low = 3.0E+00
  high = 10.0E+00
  dfn = genunf ( low, high )

  low = 5.0E+00
  high = 10.0E+00
  dfd = genunf ( low, high )

  low = 0.0E+00
  high = 2.0E+00
  xnonc = genunf ( low, high )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  N = ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Parameters:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  DFN =   ', dfn
  write ( *, '(a,g14.6)' ) '  DFD =   ', dfd
  write ( *, '(a,g14.6)' ) '  XNONC = ', xnonc
!
!  Generate N samples.
!
  do i = 1, n
    array(i) = gennf ( dfn, dfd, xnonc )
  end do
!
!  Compute statistics on the samples.
!
  call stats ( array, n, av, var, xmin, xmax )
!
!  Request expected value of statistics for this distribution.
!
  pdf = 'nf'
  param(1) = dfn
  param(2) = dfd
  param(3) = xnonc
  call trstat ( pdf, param, avtr, vartr )

  write ( *, '(a)' ) ' '
  write ( *, '(a,2g14.6)' ) '  Sample data range:          ', xmin, xmax
  write ( *, '(a,2g14.6)' ) '  Sample mean, variance:      ', av,   var
  write ( *, '(a,2g14.6)' ) '  Distribution mean, variance ', avtr, vartr

  return
end
subroutine test_gennor ( phrase )

!*****************************************************************************80
!
!! TEST_GENNOR tests GENNOR, which generates normal deviates.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  parameter ( n = 1000 )

  real ( kind = 4 ) array(n)
  real ( kind = 4 ) av
  real ( kind = 4 ) avtr
  real ( kind = 4 ) gennor
  real ( kind = 4 ) genunf
  real ( kind = 4 ) high
  integer ( kind = 4 ) i
  real ( kind = 4 ) low
  real ( kind = 4 ) mu
  real ( kind = 4 ) param(2)
  character ( len = 4 ) pdf
  character ( len = * ) phrase
  real ( kind = 4 ) sd
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2
  real ( kind = 4 ) var
  real ( kind = 4 ) vartr
  real ( kind = 4 ) xmax
  real ( kind = 4 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_GENNOR'
  write ( *, '(a)' ) '  GENNOR generates normal deviates.'
!
!  Initialize the generators.
!
  call initialize ( )
!
!  Set the seeds based on the phrase.
!
  call phrtsd ( phrase, seed1, seed2 )
!
!  Initialize all generators.
!
  call set_initial_seed ( seed1, seed2 )
!
!  Select the parameters at random within a given range.
!
  low = -10.0E+00
  high = 10.0E+00
  mu = genunf ( low, high )

  low = 0.25E+00
  high = 4.0E+00
  sd = genunf ( low, high )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  N = ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Parameters:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  MU =   ', mu
  write ( *, '(a,g14.6)' ) '  SD =   ', sd
!
!  Generate N samples.
!
  do i = 1, n
    array(i) = gennor ( mu, sd )
  end do
!
!  Compute statistics on the samples.
!
  call stats ( array, n, av, var, xmin, xmax )
!
!  Request expected value of statistics for this distribution.
!
  pdf = 'nor'
  param(1) = mu
  param(2) = sd

  call trstat ( pdf, param, avtr, vartr )

  write ( *, '(a)' ) ' '
  write ( *, '(a,2g14.6)' ) '  Sample data range:          ', xmin, xmax
  write ( *, '(a,2g14.6)' ) '  Sample mean, variance:      ', av,   var
  write ( *, '(a,2g14.6)' ) '  Distribution mean, variance ', avtr, vartr

  return
end
subroutine test_ignpoi ( phrase )

!*****************************************************************************80
!
!! TEST_IGNPOI tests IGNPOI, which generates Poisson deviates.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  parameter ( n = 1000 )

  real ( kind = 4 ) array(n)
  real ( kind = 4 ) av
  real ( kind = 4 ) avtr
  real ( kind = 4 ) genunf
  real ( kind = 4 ) high
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ignpoi
  real ( kind = 4 ) low
  real ( kind = 4 ) mu
  real ( kind = 4 ) param(1)
  character ( len = 4 ) pdf
  character ( len = * ) phrase
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2
  real ( kind = 4 ) var
  real ( kind = 4 ) vartr
  real ( kind = 4 ) xmax
  real ( kind = 4 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_IGNPOI'
  write ( *, '(a)' ) '  IGNPOI generates Poisson deviates.'
!
!  Initialize the generators.
!
  call initialize ( )
!
!  Set the seeds based on the phrase.
!
  call phrtsd ( phrase, seed1, seed2 )
!
!  Initialize all generators.
!
  call set_initial_seed ( seed1, seed2 )
!
!  Select the parameters at random within a given range.
!
  low = 0.5E+00
  high = 20.0E+00
  mu = genunf ( low, high )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  N = ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Parameters:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  MU = ', mu
!
!  Generate N samples.
!
  do i = 1, n
    array(i) = real ( ignpoi ( mu ) )
  end do
!
!  Compute statistics on the samples.
!
  call stats ( array, n, av, var, xmin, xmax )
!
!  Request expected value of statistics for this distribution.
!
  pdf = 'poi'
  param(1) = mu
  call trstat ( pdf, param, avtr, vartr )

  write ( *, '(a)' ) ' '
  write ( *, '(a,2g14.6)' ) '  Sample data range:          ', xmin, xmax
  write ( *, '(a,2g14.6)' ) '  Sample mean, variance:      ', av,   var
  write ( *, '(a,2g14.6)' ) '  Distribution mean, variance ', avtr, vartr

  return
end
subroutine test_genunf ( phrase )

!*****************************************************************************80
!
!! TEST_GENUNF tests GENUNF, which generates uniform deviates.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  parameter ( n = 1000 )

  real ( kind = 4 ) a
  real ( kind = 4 ) array(n)
  real ( kind = 4 ) av
  real ( kind = 4 ) avtr
  real ( kind = 4 ) b
  real ( kind = 4 ) genunf
  real ( kind = 4 ) high
  integer ( kind = 4 ) i
  real ( kind = 4 ) low
  real ( kind = 4 ) param(2)
  character ( len = 4 ) pdf
  character ( len = * ) phrase
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2
  real ( kind = 4 ) var
  real ( kind = 4 ) vartr
  real ( kind = 4 ) xmax
  real ( kind = 4 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_GENUNF'
  write ( *, '(a)' ) '  GENUNF generates uniform deviates.'
!
!  Initialize the generators.
!
  call initialize ( )
!
!  Set the seeds based on the phrase.
!
  call phrtsd ( phrase, seed1, seed2 )
!
!  Initialize all generators.
!
  call set_initial_seed ( seed1, seed2 )
!
!  Select the parameters at random within a given range.
!
  low = 1.0E+00
  high = 10.0E+00
  a = genunf ( low, high )

  low = a + 1.0E+00
  high = a + 10.0E+00
  b = genunf ( low, high )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  N = ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Parameters:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  A = ', a
  write ( *, '(a,g14.6)' ) '  B = ', b
!
!  Generate N samples.
!
  do i = 1, n
    array(i) = genunf ( a, b )
  end do
!
!  Compute statistics on the samples.
!
  call stats ( array, n, av, var, xmin, xmax )
!
!  Request expected value of statistics for this distribution.
!
  pdf = 'unf'
  param(1) = a
  param(2) = b
  call trstat ( pdf, param, avtr, vartr )

  write ( *, '(a)' ) ' '
  write ( *, '(a,2g14.6)' ) '  Sample data range:          ', xmin, xmax
  write ( *, '(a,2g14.6)' ) '  Sample mean, variance:      ', av,   var
  write ( *, '(a,2g14.6)' ) '  Distribution mean, variance ', avtr, vartr

  return
end
subroutine snorm_test ( )

!*****************************************************************************80
!
!! SNORM_TEST tests SNORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) test
  integer ( kind = 4 ) test_num
  real ( kind = 4 ) snorm
  real ( kind = 4 ) x

  test_num = 20

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SNORM_TEST'
  write ( *, '(a)' ) '  SNORM generates normally distributed random values.'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    x = snorm ( )
    write ( *, '(g14.6)' ) x
  end do

  return
end
