program main

!*****************************************************************************80
!
!! MAIN is the main program for BLACK_SCHOLES_TEST.
!
!  Discussion:
!
!    BLACK_SCHOLES_TEST tests the BLACK_SCHOLES library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( );
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLACK_SCHOLES_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the BLACK_SCHOLES library.'

  call asset_path_test ( )
  call binomial_test ( )
  call bsf_test ( )
  call forward_test ( )
  call mc_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLACK_SCHOLES_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine asset_path_test ( )

!*****************************************************************************80
!
!! ASSET_PATH_TEST tests ASSET_PATH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 June 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 100

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  real ( kind = 8 ) s(0:n)
  real ( kind = 8 ) s0
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) t1

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ASSET_PATH_TEST:'
  write ( *, '(a)' ) '  Demonstrate the simulated of an asset price path.'

  s0 = 2.0D+00
  mu = 0.1D+00
  sigma = 0.3D+00
  t1 = 1.0D+00
  seed = 123456789

  write ( *, '(a,g14.6)' ) ' '
  write ( *, '(a,g14.6)' ) '  The asset price at time 0      S0    = ', s0
  write ( *, '(a,g14.6)' ) '  The asset expected growth rate MU    = ', mu
  write ( *, '(a,g14.6)' ) '  The asset volatility           SIGMA = ', sigma
  write ( *, '(a,g14.6)' ) '  The expiry date                T1    = ', t1
  write ( *, '(a,i6)' )    '  The number of time steps       N     = ', n
  write ( *, '(a,i12)' )   '  The random number seed was     SEED  = ', seed

  call asset_path ( s0, mu, sigma, t1, n, seed, s )

  call r8vec_print_part ( n + 1, s, 10, '  Partial results:' )

  data_filename = 'asset_path_data.txt'
  call get_unit ( data_unit )
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 0, n
    write ( data_unit, '(i6,2x,g14.6)' ) i, s(i)
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Created graphics data file "' // trim ( data_filename ) // '".'

  command_filename = 'asset_path_commands.txt'
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "asset_path.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Time"'
  write ( command_unit, '(a)' ) 'set ylabel "Value"'
  write ( command_unit, '(a)' ) 'set title "Asset value over time"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "blue"'
  write ( command_unit, '(a)' ) 'quit'
  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  return
end
subroutine binomial_test ( )

!*****************************************************************************80
!
!! BINOMIAL_TEST tests BINOMIAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 February 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) c
  real ( kind = 8 ) e
  integer ( kind = 4 ) m
  real ( kind = 8 ) r
  real ( kind = 8 ) s0
  real ( kind = 8 ) sigma
  real ( kind = 8 ) t1

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BINOMIAL_TEST:'
  write ( *, '(a)' ) '  A demonstration of the binomial method'
  write ( *, '(a)' ) '  for option valuation.'

  s0 = 2.0D+00
  e = 1.0D+00
  r = 0.05D+00
  sigma = 0.25D+00
  t1 = 3.0D+00
  m = 256

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  The asset price at time 0 S0    = ', s0
  write ( *, '(a,g14.6)' ) '  The exercise price        E     = ', e
  write ( *, '(a,g14.6)' ) '  The interest rate         R     = ', r
  write ( *, '(a,g14.6)' ) '  The asset volatility      SIGMA = ', sigma
  write ( *, '(a,g14.6)' ) '  The expiry date           T1    = ', t1
  write ( *, '(a,i8)' )    '  The number of intervals   M     = ', m

  call binomial ( s0, e, r, sigma, t1, m, c )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  The option value is ', c

  return
end
subroutine bsf_test ( )

!*****************************************************************************80
!
!! BSF_TEST tests BSF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) c
  real ( kind = 8 ) e
  real ( kind = 8 ) r
  real ( kind = 8 ) s0
  real ( kind = 8 ) sigma
  real ( kind = 8 ) t0
  real ( kind = 8 ) t1

  write ( *, '(a)' )
  write ( *, '(a)' ) 'BSF_TEST:'
  write ( *, '(a)' ) '  A demonstration of the Black-Scholes formula'
  write ( *, '(a)' ) '  for option valuation.'

  s0 = 2.0D+00
  t0 = 0.0D+00
  e = 1.0D+00
  r = 0.05D+00
  sigma = 0.25D+00
  t1 = 3.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  The asset price at time T0 S0    = ', s0
  write ( *, '(a,g14.6)' ) '  The time                   T0    = ', t0
  write ( *, '(a,g14.6)' ) '  The exercise price         E     = ', e
  write ( *, '(a,g14.6)' ) '  The interest rate          R     = ', r
  write ( *, '(a,g14.6)' ) '  The asset volatility       SIGMA = ', sigma
  write ( *, '(a,g14.6)' ) '  The expiry date            T1    = ', t1

  call bsf ( s0, t0, e, r, sigma, t1, c )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  The option value C = ', c

  return
end
subroutine forward_test ( )

!*****************************************************************************80
!
!! FORWARD_TEST tests FORWARD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) e
  integer ( kind = 4 ) i
  integer ( kind = 4 ) nt
  integer ( kind = 4 ) nx
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  real ( kind = 8 ) sigma
  real ( kind = 8 ) smax
  real ( kind = 8 ) smin
  real ( kind = 8 ) t1
  real ( kind = 8 ), allocatable :: u(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FORWARD_TEST:'
  write ( *, '(a)' ) '  A demonstration of the forward difference method'
  write ( *, '(a)' ) '  for option valuation.'

  e = 4.0D+00
  r = 0.03D+00
  sigma = 0.50D+00
  t1 = 1.0D+00
  nx = 11
  nt = 29
  smax = 10.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  The exercise price        E =     ', e
  write ( *, '(a,g14.6)' ) '  The interest rate         R =     ', r
  write ( *, '(a,g14.6)' ) '  The asset volatility      SIGMA = ', sigma;
  write ( *, '(a,g14.6)' ) '  The expiry date           T1 =    ', t1
  write ( *, '(a,i8)' ) '  The number of space steps NX =    ', nx
  write ( *, '(a,i8)' ) '  The number of time steps  NT =    ', nt
  write ( *, '(a,g14.6)' ) '  The value of              SMAX =  ', smax

  allocate ( u(nx-1,nt+1) )

  call forward ( e, r, sigma, t1, nx, nt, smax, u )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Initial   Option'
  write ( *, '(a)' ) '  Value     Value' 
  write ( *, '(a)' ) ' '

  smin = 0.0D+00
  do i = 1, nx - 1 
    s = ( ( nx - i - 1 ) * smin + i * smax ) / real ( nx - 1, kind = 8 )
    write ( *, '(2x,g14.6,2x,g14.6)' ) s, u(i,nt+1)
  end do

  deallocate ( u )

  return
end
subroutine mc_test ( )

!*****************************************************************************80
!
!! MC_TEST tests MC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) conf(2)
  real ( kind = 8 ) e
  integer ( kind = 4 ) m
  real ( kind = 8 ) r
  real ( kind = 8 ) s0
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) t1

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MC_TEST:'
  write ( *, '(a)' ) '  A demonstration of the Monte Carlo method'
  write ( *, '(a)' ) '  for option valuation.'

  s0 = 2.0D+00
  e = 1.0D+00
  r = 0.05D+00
  sigma = 0.25D+00
  t1 = 3.0D+00
  m = 1000000
  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a, g14.6)' ) '  The asset price at time 0, S0    = ', s0
  write ( *, '(a, g14.6)' ) '  The exercise price         E     = ', e
  write ( *, '(a, g14.6)' ) '  The interest rate          R     = ', r
  write ( *, '(a, g14.6)' ) '  The asset volatility       SIGMA = ', sigma
  write ( *, '(a, g14.6)' ) '  The expiry date            T1    = ', t1
  write ( *, '(a, i8)' )    '  The number of simulations  M     = ', m

  call mc ( s0, e, r, sigma, t1, m, seed, conf )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6,a,g14.6,a)' ) &
    '  The confidence interval is [', conf(1), ',', conf(2), '].'

  return
end
