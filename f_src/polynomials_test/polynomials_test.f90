program main

!*****************************************************************************80
!
!! MAIN is the main program for POLYNOMIALS_TEST.
!
!  Discussion:
!
!    POLYNOMIALS_TEST tests the POLYNOMIALS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: f(:)
  integer ( kind = 4 ) i
  real ( kind = 8 ), allocatable :: l(:)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_log_2
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: u(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIALS_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the POLYNOMIALS library.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RANGE_BY_SAMPLING_TEST:'
  write ( *, '(a)' ) '  Use N sample values of each polynomial over its domain to estimate'
  write ( *, '(a)' ) '  its minimum Pmin and maximum Pmax'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N           Pmin             Pmax'

  do i = 1, 28

    write ( *, '(a)' ) ''

    if ( i == 1 ) then
      call butcher_m ( m )
    else if ( i == 2 ) then
      call camel_m ( m )
    else if ( i == 3 ) then
      call camera_m ( m )
    else if ( i == 4 ) then
      call caprasse_m ( m )
    else if ( i == 5 ) then
      call cyclic5_m ( m )
    else if ( i == 6 ) then
      call cyclic7_m ( m )
    else if ( i == 7 ) then
      call cyclic8_m ( m )
    else if ( i == 8 ) then
      call goldstein_price_m ( m )
    else if ( i == 9 ) then
      call hairer_m ( m )
    else if ( i == 10 ) then
      call heart_m ( m )
    else if ( i == 11 ) then
      call himmelblau_m ( m )
    else if ( i == 12 ) then
      call hunecke_m ( m )
    else if ( i == 13 ) then
      call kearfott_m ( m )
    else if ( i == 14 ) then
      call lv3_m ( m )
    else if ( i == 15 ) then
      call lv4_m ( m )
    else if ( i == 16 ) then
      call magnetism6_m ( m )
    else if ( i == 17 ) then
      call magnetism7_m ( m )
    else if ( i == 18 ) then
      call quadratic_m ( m )
    else if ( i == 19 ) then
      call rd_m ( m )
    else if ( i == 20 ) then
      call reimer5_m ( m )
    else if ( i == 21 ) then
      call reimer6_m ( m )
    else if ( i == 22 ) then
      call rosenbrock_m ( m )
    else if ( i == 23 ) then
      call schwefel_m ( m )
    else if ( i == 24 ) then
      call smith1_m ( m )
    else if ( i == 25 ) then
      call smith2_m ( m )
    else if ( i == 26 ) then
      call virasoro_m ( m )
    else if ( i == 27 ) then
      call wright_m ( m )
    else if ( i == 28 ) then
      call zakharov_m ( m )
    end if

    allocate ( l(1:m) )
    allocate ( u(1:m) )

    if ( i == 1 ) then
      call  butcher_b ( m, l, u )
      write ( *, '(a)' ) '  butcher: [-1.4393333333, +0.219]'
    else if ( i == 2 ) then
      call  camel_b ( m, l, u )
      write ( *, '(a)' ) '  camel: [ -1.031628453489616, ? ]:'
    else if ( i == 3 ) then
      call  camera_b ( m, l, u )
      write ( *, '(a)' ) '  camera: [-270397.4, +270202.6]'
    else if ( i == 4 ) then
      call  caprasse_b ( m, l, u )
      write ( *, '(a)' ) '  caprasse: [-3.1800966258, +4.4852773332]'
    else if ( i == 5 ) then
      call  cyclic5_b ( m, l, u )
      write ( *, '(a)' ) '  cyclic5: [-30000, +50000]'
    else if ( i == 6 ) then
      call  cyclic7_b ( m, l, u )
      write ( *, '(a)' ) '  cyclic7: [-5.0, +7.0]'
    else if ( i == 7 ) then
      call  cyclic8_b ( m, l, u )
      write ( *, '(a)' ) '  cyclic8: [-8.0, +8.0]'
    else if ( i == 8 ) then
      call  goldstein_price_b ( m, l, u )
      write ( *, '(a)' ) '  goldstein_price: [ 3, ? ]:'
    else if ( i == 9 ) then
      call  hairer_b ( m, l, u )
      write ( *, '(a)' ) '  hairer: [-1875.25, -48.25]'
    else if ( i == 10 ) then
      call  heart_b ( m, l, u )
      write ( *, '(a)' ) '  heart: [-1.36775, +1.74345327935'
    else if ( i == 11 ) then
      call  himmelblau_b ( m, l, u )
      write ( *, '(a)' ) '  himmelblau: [ 0, ? ]:'
    else if ( i == 12 ) then
      call  hunecke_b ( m, l, u )
      write ( *, '(a)' ) '  hunecke: [-1436.515078155, +161.120543283]'
    else if ( i == 13 ) then
      call  kearfott_b ( m, l, u )
      write ( *, '(a)' ) '  kearfott: [ 0, ? ]:'
    else if ( i == 14 ) then
      call  lv3_b ( m, l, u )
      write ( *, '(a)' ) '  lv3: [-9.35, +14.8 ]'
    else if ( i == 15 ) then
      call  lv4_b ( m, l, u )
      write ( *, '(a)' ) '  lv4: [-20.8, +22.8]'
    else if ( i == 16 ) then
      call  magnetism6_b ( m, l, u )
      write ( *, '(a)' ) '  magnetism6: [-0.25, +280.0]'
    else if ( i == 17 ) then
      call  magnetism7_b ( m, l, u )
      write ( *, '(a)' ) '  magnetism7: [-0.25, +330.0]'
    else if ( i == 18 ) then
      call  quadratic_b ( m, l, u )
      write ( *, '(a)' ) '  quadratic: [ -2, ? ]:'
    else if ( i == 19 ) then
      call  rd_b ( m, l, u )
      write ( *, '(a)' ) '  rd: [-36.71269068, +10.40560403]'
    else if ( i == 20 ) then
      call  reimer5_b ( m, l, u )
      write ( *, '(a)' ) '  reimer5: [-5.0, +5.0]'
    else if ( i == 21 ) then
      call  reimer6_b ( m, l, u )
      write ( *, '(a)' ) '  reimer6: [-937501, +937499]'
    else if ( i == 22 ) then
      call  rosenbrock_b ( m, l, u )
      write ( *, '(a)' ) '  rosenbrock: [ 0, ? ]:'
    else if ( i == 23 ) then
      call  schwefel_b ( m, l, u )
      write ( *, '(a)' ) '  schwefel: [ 0, ? ]:'
    else if ( i == 24 ) then
      call  smith1_b ( m, l, u )
      write ( *, '(a)' ) '  smith1: [ ?, ? ]:'
    else if ( i == 25 ) then
      call  smith2_b ( m, l, u )
      write ( *, '(a)' ) '  smith2: [ ?, ? ]:'
    else if ( i == 26 ) then
      call  virasoro_b ( m, l, u )
      write ( *, '(a)' ) '  virasoro: [-29.0, +21.0]'
    else if ( i == 27 ) then
      call  wright_b ( m, l, u )
      write ( *, '(a)' ) '  wright: [-30.25, 40.0 ]'
    else if ( i == 28 ) then
      call  zakharov_b ( m, l, u )
      write ( *, '(a)' ) '  zakharov: [ 0, ? ]:'
    end if

    seed = 123456789

    n = 8

    do n_log_2 = 4, 20

      n = n * 2

      allocate ( x(1:m,1:n) )
      allocate ( f(1:n) )

      call r8mat_uniform_abvec ( m, n, u, l, seed, x )

      if ( i == 1 ) then
        call butcher_f ( m, n, x, f )
      else if ( i == 2 ) then
        call camel_f ( m, n, x, f )
      else if ( i == 3 ) then
        call camera_f ( m, n, x, f )
      else if ( i == 4 ) then
        call caprasse_f ( m, n, x, f )
      else if ( i == 5 ) then
        call cyclic5_f ( m, n, x, f )
      else if ( i == 6 ) then
        call cyclic7_f ( m, n, x, f )
      else if ( i == 7 ) then
        call cyclic8_f ( m, n, x, f )
      else if ( i == 8 ) then
        call goldstein_price_f ( m, n, x, f )
      else if ( i == 9 ) then
        call hairer_f ( m, n, x, f )
      else if ( i == 10 ) then
        call heart_f ( m, n, x, f )
      else if ( i == 11 ) then
        call himmelblau_f ( m, n, x, f )
      else if ( i == 12 ) then
        call hunecke_f ( m, n, x, f )
      else if ( i == 13 ) then
        call kearfott_f ( m, n, x, f )
      else if ( i == 14 ) then
        call lv3_f ( m, n, x, f )
      else if ( i == 15 ) then
        call lv4_f ( m, n, x, f )
      else if ( i == 16 ) then
        call magnetism6_f ( m, n, x, f )
      else if ( i == 17 ) then
        call magnetism7_f ( m, n, x, f )
      else if ( i == 18 ) then
        call quadratic_f ( m, n, x, f )
      else if ( i == 19 ) then
        call rd_f ( m, n, x, f )
      else if ( i == 20 ) then
        call reimer5_f ( m, n, x, f )
      else if ( i == 21 ) then
        call reimer6_f ( m, n, x, f )
      else if ( i == 22 ) then
        call rosenbrock_f ( m, n, x, f )
      else if ( i == 23 ) then
        call schwefel_f ( m, n, x, f )
      else if ( i == 24 ) then
        call smith1_f ( m, n, x, f )
      else if ( i == 25 ) then
        call smith2_f ( m, n, x, f )
      else if ( i == 26 ) then
        call virasoro_f ( m, n, x, f )
      else if ( i == 27 ) then
        call wright_f ( m, n, x, f )
      else if ( i == 28 ) then
        call zakharov_f ( m, n, x, f )
      end if

      write ( *, '(2x,i8,2x,g16.8,2x,g16.8)' ) n, minval ( f ), maxval ( f )

      deallocate ( x )
      deallocate ( f )

    end do

    deallocate ( l )
    deallocate ( u )

  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIALS_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end

