program main

!*******************************************************************************
!
!! main is the main program for sncndn_test.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2020
!
!  Author:
!
!    John Burkardt
!
  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'sncndn_test:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  sncndn evaluates Jacobi elliptic functions.'

  call jacobi_cn_test ( )
  call jacobi_dn_test ( )
  call jacobi_sn_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'sncndn_test:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  sncndn evaluates Jacobi elliptic functions.'
  call timestamp ( )

  stop 0
end
subroutine jacobi_cn_test ( )

!*******************************************************************************
!
!! jacobi_cn_test tests jacobi_cn.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cn1
  real ( kind = 8 ) cn2
  real ( kind = 8 ) jacobi_cn
  real ( kind = 8 ) m
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) u

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'jacobi_cn_test:'
  write ( *, '(a)' ) '  jacobi_cn() evaluates the Jacobi elliptic function CN.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    U       M       Exact CN                CN(U,M)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call jacobi_cn_values ( n_data, u, m, cn1 )

    if ( n_data == 0 ) then
      exit
    end if

    cn2 = jacobi_cn ( u, m )

    write ( *, '(2f8.4,2g24.16)' ) u, m, cn1, cn2

  end do

  return
end
subroutine jacobi_dn_test ( )

!*******************************************************************************
!
!! jacobi_dn_test tests jacobi_dn.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) dn1
  real ( kind = 8 ) dn2
  real ( kind = 8 ) jacobi_dn
  real ( kind = 8 ) m
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) u

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'jacobi_dn_test:'
  write ( *, '(a)' ) '  jacobi_dn() evaluates the Jacobi elliptic function DN.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    U       M       Exact DN                DN(U,M)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call jacobi_dn_values ( n_data, u, m, dn1 )

    if ( n_data == 0 ) then
      exit
    end if

    dn2 = jacobi_dn ( u, m )

    write ( *, '(2f8.4,2g24.16)' ) u, m, dn1, dn2

  end do

  return
end
subroutine jacobi_sn_test ( )

!*******************************************************************************
!
!! jacobi_sn_test tests jacobi_sn.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) m
  real ( kind = 8 ) jacobi_sn
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) sn1
  real ( kind = 8 ) sn2
  real ( kind = 8 ) u

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'jacobi_sn_test:'
  write ( *, '(a)' ) '  jacobi_sn() evaluates the Jacobi elliptic function SN.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    U       M       Exact SN                SN(U,M)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call jacobi_sn_values ( n_data, u, m, sn1 )

    if ( n_data == 0 ) then
      exit
    end if

    sn2 = jacobi_sn ( u, m )

    write ( *, '(2f8.4,2g24.16)' ) u, m, sn1, sn2

  end do

  return
end
