module covariance

!*****************************************************************************80
!
!! COVARIANCE is a module that sets up and retains covariance information.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 June 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), save, allocatable :: c(:,:)
  real ( kind = 8 ), save :: c_det = 0.0D+00
  real ( kind = 8 ), save, allocatable :: c_factor(:,:)
  logical, save :: c_factored = .false.
  real ( kind = 8 ), save, allocatable :: zp_mean(:)

  contains

subroutine covariance_initialize ( par_num )

!*****************************************************************************80
!
!! COVARIANCE_INITIALIZE sets covariance matrix, Cholesky factor, determinant.
!
!  Discussion:
!
!    Note that C_FACTOR is the upper triangular Cholesky factor of the
!    covariance matrix C, so that 
!
!      C = C_FACTOR' * C_FACTOR
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) PAR_NUM, the number of parameters.
!
  implicit none

  integer ( kind = 4 ) par_num

  allocate ( c(1:par_num,1:par_num) )
  call covariance_set ( par_num, c )

  allocate ( c_factor(1:par_num,1:par_num) )
  call r8po_fa ( par_num, c, c_factor )

  call r8po_det ( par_num, c_factor, c_det )

  allocate ( zp_mean(1:par_num) )
  zp_mean(1:par_num) = 0.0D+00

  c_factored = .true.

  return
end subroutine covariance_initialize

subroutine covariance_set ( par_num, c )

!*****************************************************************************80
!
!! COVARIANCE_SET sets the covariance matrix.
!
!  Discussion:
!
!    This is a multivariate normal distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jasper Vrugt, CJF ter Braak, CGH Diks, Bruce Robinson, James Hyman, 
!    Dave Higdon,
!    Accelerating Markov Chain Monte Carlo Simulation by Differential 
!    Evolution with Self-Adaptive Randomized Subspace Sampling,
!    International Journal of Nonlinear Sciences and Numerical Simulation,
!    Volume 10, Number 3, March 2009, pages 271-288.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) PAR_NUM, the total number of parameters.
!    1 <= PAR_NUM.
!
!    Output, real ( kind = 8 ) C(PAR_NUM,PAR_NUM), the covariance matrix.
!
  implicit none

  integer ( kind = 4 ) par_num

  real ( kind = 8 ) c(par_num,par_num)
  integer ( kind = 4 ) i

  c(1:par_num,1:par_num) = 0.5D+00

  do i = 1, par_num
    c(i,i) = real ( i, kind = 8 )
  end do

  return
end subroutine covariance_set

end module covariance

subroutine problem_size ( chain_num, cr_num, gen_num, pair_num, par_num )

!*****************************************************************************80
!
!! PROBLEM_SIZE sets information having to do with dimensions.
!
!  Discussion:
!
!    In the Vrugt paper, PAR_NUM is 100.  For testing, it is reasonable
!    to try a tiny value like PAR_NUM = 5 instead.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 June 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jasper Vrugt, CJF ter Braak, CGH Diks, Bruce Robinson, James Hyman, 
!    Dave Higdon,
!    Accelerating Markov Chain Monte Carlo Simulation by Differential 
!    Evolution with Self-Adaptive Randomized Subspace Sampling,
!    International Journal of Nonlinear Sciences and Numerical Simulation,
!    Volume 10, Number 3, March 2009, pages 271-288.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) CHAIN_NUM, the total number of chains.
!    3 <= CHAIN_NUM.
!
!    Output, integer ( kind = 4 ) CR_NUM, the total number of CR values.
!    1 <= CR_NUM.
!
!    Output, integer ( kind = 4 ) GEN_NUM, the total number of generations.
!    2 <= GEN_NUM.
!
!    Output, integer ( kind = 4 ) PAIR_NUM, the number of pairs of 
!    crossover chains.
!    0 <= PAIR_NUM.
!
!    Output, integer ( kind = 4 ) PAR_NUM, the total number of parameters.
!    1 <= PAR_NUM.
!
  use covariance

  implicit none

  integer ( kind = 4 ) chain_num
  integer ( kind = 4 ) cr_num
  integer ( kind = 4 ) gen_num
  integer ( kind = 4 ) pair_num
  integer ( kind = 4 ) par_num

  chain_num = 10
  cr_num = 3
  gen_num = 10
  pair_num = 3
! par_num = 100
  par_num = 5
!
!  Initialize the covariance information.
!
  call covariance_initialize ( par_num )

  return
end
subroutine problem_value ( chain_filename, gr_filename, gr_threshold, &
  jumpstep, limits, par_num, printstep, restart_read_filename, &
  restart_write_filename  )

!*****************************************************************************80
!
!! PROBLEM_VALUE sets information, including numeric data.
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
!  Parameters:
!
!    Output, character ( len = * ) CHAIN_FILENAME, the "base" filename
!    to be used for the chain files.  If this is the empty string '',
!    then the chain files will not be written.  This name should 
!    include a string of 0's which will be replaced by the chain 
!    indices.  For example, "chain000.txt" would work as long as the
!    number of chains was 1000 or less.
!
!    Output, character ( len = * ) GR_FILENAME, the name of the file
!    in which values of the Gelman-Rubin statistic will be recorded,
!    or '' if this file is not to be written.
!
!    Output, real ( kind = 8 ) GR_THRESHOLD, the convergence tolerance for
!    the Gelman-Rubin statistic.
!
!    Output, integer ( kind = 4 ) JUMPSTEP, forces a "long jump" every
!    JUMPSTEP generations.
!
!    Output, real ( kind = 8 ) LIMITS(2,PAR_NUM), lower and upper bounds
!    for each parameter.
!
!    Input, integer ( kind = 4 ) PAR_NUM, the total number of parameters.
!    1 <= PAR_NUM.
!
!    Output, integer ( kind = 4 ) PRINTSTEP, the interval between generations 
!    on which the Gelman-Rubin statistic will be computed and written to a file.
!
!    Output, character ( len = * ) RESTART_READ_FILENAME, the name of the file
!    containing restart information, or '' if this is not a restart run.
!
!    Output, character ( len = * ) RESTART_WRITE_FILENAME, the name of the file
!    to be written, containing restart information, or '' if a restart file 
!    is not to be written.
!
  implicit none

  integer ( kind = 4 ) par_num

  character ( len = * ) chain_filename
  character ( len = * ) gr_filename
  real ( kind = 8 ) gr_threshold
  integer ( kind = 4 ) jumpstep
  real ( kind = 8 ) limits(2,par_num)
  integer ( kind = 4 ) printstep
  character ( len = * ) restart_read_filename
  character ( len = * ) restart_write_filename

  chain_filename = 'problem1_chain00.txt'
  gr_filename = 'problem1_gr.txt'
  gr_threshold = 1.2D+00
  jumpstep = 5
  limits(1,1:par_num) =   9.9D+00
  limits(2,1:par_num) = +10.0D+00
  printstep = 10
  restart_read_filename = ''
  restart_write_filename = 'problem1_restart.txt'

  return
end
function prior_density ( par_num, zp )

!*****************************************************************************80
!
!! PRIOR_DENSITY evaluates the prior density function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) PAR_NUM, the total number of parameters.
!    1 <= PAR_NUM.
!
!    Input, real ( kind = 8 ) ZP(PAR_NUM), the argument of the density
!    function.
!
!    Output, real ( kind = 8 ) PRIOR_DENSITY, the value of the prior
!    density function.
!
  use covariance

  implicit none

  integer ( kind = 4 ) par_num

  real ( kind = 8 ) prior_density
  real ( kind = 8 ) r8vec_multinormal_pdf
  real ( kind = 8 ) zp(par_num)

  prior_density = r8vec_multinormal_pdf ( par_num, zp_mean, c_factor, &
    c_det, zp )

  return
end
subroutine prior_sample ( par_num, zp )

!*****************************************************************************80
!
!! PRIOR_SAMPLE samples from the prior distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) PAR_NUM, the total number of parameters.
!    1 <= PAR_NUM.
!
!    Output, real ( kind = 8 ) ZP(PAR_NUM), the sample from the distribution.
!
  use covariance

  implicit none

  integer ( kind = 4 ) par_num

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_normal_01_sample
  real ( kind = 8 ) x(par_num)
  real ( kind = 8 ) zp(par_num)

  do i = 1, par_num
    x(i) = r8_normal_01_sample ( )
  end do

  zp(1:par_num) = zp_mean(1:par_num) &
    + matmul ( transpose ( c_factor(1:par_num,1:par_num) ), x(1:par_num) )

  return
end
function sample_likelihood ( par_num, zp )

!*****************************************************************************80
!
!! SAMPLE_LIKELIHOOD computes the log likelihood function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) PAR_NUM, the total number of parameters.
!    1 <= PAR_NUM.
!
!    Input, real ( kind = 8 ) ZP(PAR_NUM), a sample.
!
!    Output, real ( kind = 8 ) SAMPLE_LIKELIHOOD, the log likelihood function 
!    for the sample.
!
  use covariance

  implicit none

  integer ( kind = 4 ) par_num

  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) sample_likelihood
  real ( kind = 8 ) x(par_num)
  real ( kind = 8 ) xcx
  real ( kind = 8 ) y(par_num)
  real ( kind = 8 ) zp(par_num)

  x(1:par_num) = zp(1:par_num) - zp_mean(1:par_num)

  call r8ut_sl ( par_num, c_factor, x, y )
!
!  Compute:
!    (x-mu)' * inv(C)          * (x-mu)
!  = (x-mu)' * inv(R'*R)       * (x-mu)
!  = (x-mu)' * inv(R) * inv(R) * (x-mu)
!  = y' * y.
!
  xcx = dot_product ( y, y )

  sample_likelihood = &
    - 0.5D+00 * real ( par_num, kind = 8 ) * log ( 2.0D+00 * r8_pi ) &
    - 0.5D+00 * log ( c_det ) &
    - 0.5D+00 * xcx

  return
end

