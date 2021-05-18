program main

!******************************************************************************/
!
!! polyominoes_test tests polyominoes.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'polyominoes_test'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test polyominoes().'

  call pentominoes_test ( )
  call polyomino_condense_test ( )
  call polyomino_embed_test ( )
  call polyomino_enumerate_test ( )
  call polyomino_index_test ( )
  call polyomino_lp_write_test ( )
  call polyomino_transform_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'polyominoes_test'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  return
end
subroutine pentominoes_test ( )

!******************************************************************************/
!
!! PENTOMINOES_TEST tests the PENTOMINOES library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PENTOMINOES_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the PENTOMINOES library.'

  call pentomino_matrix_test ( )
  call pentomino_plot_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PENTOMINOES_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  return
end
subroutine pentomino_matrix_test ( )

!*****************************************************************************80
!
!! PENTOMINO_MATRIX_TEST tests PENTOMINO_MATRIX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  character ( len = 1 ) name
  integer ( kind = 4 ) p(5*5)
  integer ( kind = 4 ) p_m
  integer ( kind = 4 ) p_n
  character ( len = 1 ) :: pentominoes(12) = (/ &
    'F', 'I', 'L', 'N', 'P', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PENTOMINO_MATRIX_TEST'
  write ( *, '(a)' ) '  PENTOMINO_MATRIX returns a 0/1 matrix representing a pentomino.'

  do k = 1, 12
    name = pentominoes(k)
    call pentomino_matrix ( name, p_m, p_n, p )
    write ( *, '(a)' ) ''
    write ( *, '(a,i2,a,i2,a)' ) &
      '  // trim ( name ) // pentomino (', p_m, ',', p_n, ')'
    write ( *, '(a)' ) ''
    do i = 1, p_m
      write ( *, '(a)', advance = 'no' ) '    '
      do j = 1, p_n
        write ( *, '(i1)', advance = 'no' ) p(i+j*p_m)
      end do
      write ( *, '(a)' )
    end do
  end do

  return
end
subroutine pentomino_plot_test ( )

!*****************************************************************************80
!
!! PENTOMINO_PLOT_TEST tests PENTOMINO_PLOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) k
  character ( len = 1 ) name
  integer ( kind = 4 ) p(5*5)
  integer ( kind = 4 ) p_m
  integer ( kind = 4 ) p_n
  character ( len = 1 ) :: pentominoes(12) = (/ &
    'F', 'I', 'L', 'N', 'P', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PENTOMINO_PLOT_TEST'
  write ( *, '(a)' ) '  PENTOMINO_PLOT plots a pentomino.'

  do k = 1, 12
    name = pentominoes(k)
    call pentomino_matrix ( name, p_m, p_n, p )
    call pentomino_plot ( p_m, p_n, p, name )
  end do

  return
end
subroutine polyomino_condense_test ( )

!*****************************************************************************80
!
!! POLYOMINO_CONDENSE_TEST tests POLYOMINO_CONDENSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Local, integer ( kind = 4 ) MP, NP, the number of rows and columns in the 
!    representation of the polyomino P.
!
!    Local, integer ( kind = 4 ) P(MP*NP), a matrix representing the polyomino.  
!
  implicit none

  integer ( kind = 4 ) mp
  integer ( kind = 4 ) np
  integer ( kind = 4 ) :: p1(9) = (/ 0, 1, 1, 1, 1, 0, 0, 1, 0 /)
  integer ( kind = 4 ) :: p2(9) = (/ 0, 1, 2, 1, 3, 0, 0, -9, 0 /)
  integer ( kind = 4 ) :: p3(12) = (/ 0, 0, 0, 0, 1, 3, 0, 0, 0, 0, 0, 0 /)
  integer ( kind = 4 ) :: p4(8) = (/ 0, 0, 0, 0, 0, 0, 0, 0 /)

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_CONDENSE_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  POLYOMINO_CONDENSE "cleans up" a matrix that is supposed'
  write ( *, '(a)' ) '  to represent a polyomino:'
  write ( *, '(a)' ) '  * nonzero entries are set to 1'
  write ( *, '(a)' ) '  * initial and final zero rows and columns are deleted.'
!
!  Nothing happens:
!
  mp = 3
  np = 3
  call polyomino_condense_demo ( mp, np, p1 )
!
!  Nonzero, but non-one entries are set to 1.
!
  mp = 3
  np = 3
  call polyomino_condense_demo ( mp, np, p2 )
!
!  Extraneous zero rows and columns are removed.
!
  mp = 3
  np = 4
  call polyomino_condense_demo ( mp, np, p3 )
!
!  Null matrices are detected.
!
  mp = 2
  np = 4
  call polyomino_condense_demo ( mp, np, p4 )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_CONDENSE_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  return
end
subroutine polyomino_condense_demo ( mp, np, p )

!*****************************************************************************80
!
!! polyomino_condense_demo demonstrates the result of calling POLYOMINO_CONDENSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MP, NP, the number of rows and columns in the representation
!    of the polyomino P.
!
!    Input, integer ( kind = 4 ) P(MP*NP), a matrix representing the polyomino.  
!
!    Local, integer ( kind = 4 ) MQ, NQ, the number of rows and columns in the representation
!    of the condensed polyomino Q.
!
!    Local, integer ( kind = 4 ) Q(MQ*NQ), a matrix representing the condensed polyomino.  
!
  implicit none

  character ( len = 80 ) label
  integer ( kind = 4 ) mp
  integer ( kind = 4 ) mq
  integer ( kind = 4 ) np
  integer ( kind = 4 ) nq
  integer ( kind = 4 ) p(mp*np)
  integer ( kind = 4 ) q(mp*np)

  write ( label, '(a,i2,a,i2,a)' ) '  The initial (', mp, ',', np, ') polynomino P:'
  call polyomino_print ( mp, np, p, label )

  call polyomino_condense ( mp, np, p, mq, nq, q )

  write ( label, '(a,i2,a,i2,a)' ) '  The condensed (', mq, ',', nq, ') polynomino Q:'
  call polyomino_print ( mq, nq, q, label )

  return
end
subroutine polyomino_embed_test ( )

!*****************************************************************************80
!
!! POLYOMINO_EMBED_TEST tests POLYOMINO_EMBED.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_EMBED_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the POLYOMINO_EMBED library.'

  call polyomino_embed_number_test ( )
  call polyomino_embed_list_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_EMBED_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  return
end
subroutine polyomino_embed_list_test ( )

!*****************************************************************************80
!
!! POLYOMINO_EMBED_LIST_TEST tests POLYOMINO_EMBED_LIST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) embed_number
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ), allocatable :: list(:,:)
  integer ( kind = 4 ) mk
  integer ( kind = 4 ) nk
  integer ( kind = 4 ) :: mp = 3
  integer ( kind = 4 ) mq
  integer ( kind = 4 ) :: mr = 4
  integer ( kind = 4 ) :: np = 2
  integer ( kind = 4 ) nq
  integer ( kind = 4 ) :: nr = 4

  integer ( kind = 4 ) :: p(3,2) = reshape ( (/ &
    0, 0, 1, &
    1, 1, 1 /), (/ 3, 2 /) )
  integer ( kind = 4 ) q(4,4)
  integer ( kind = 4 ) :: r(4,4) = reshape ( (/ &
    0, 1, 1, 1, &
    1, 1, 1, 0, &
    1, 0, 1, 1, &
    1, 1, 1, 1 /), (/ 4, 4 /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_EMBED_LIST_TEST:'
  write ( *, '(a)' ) '  POLYOMINO_EMBED_LIST lists the offsets used'
  write ( *, '(a)' ) '  to embed a fixed polyomino in a region.'

  call polyomino_print ( mr, nr, r, '  The given region R:' )

  call polyomino_print ( mp, np, p, '  The given polyomino P:' )
!
!  Get the number of embeddings.
!
  call polyomino_embed_number ( mr, nr, r, mp, np, p, embed_number )

  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a)' ) '  As a fixed polyomino, P can be embedded in R in ', embed_number, ' ways.'
!
!  Get the list of embeddings.
!
  allocate ( list(1:embed_number,1:2) )

  call polyomino_embed_list ( mr, nr, r, mp, np, p, embed_number, list )

  do k = 1, embed_number
    mk = list(k,1)
    nk = list(k,2)
    mq = mr
    nq = nr
    q(1:mq,1:nq) = r(1:mr,1:nr)

    q(1+mk:mp+mk,1+nk:np+nk) = q(1+mk:mp+mk,1+nk:np+nk) + p(1:mp,1:np)

    write ( *, '(a)' ) ''
    write ( *, '(a,i2)' ) '  Embedding number ', k
    write ( *, '(a)' ) ''
    do i = 1, mq
      do j = 1, nq
        write ( *, '(1x,i1)', advance = 'no' ) q(i,j)
      end do
      write ( *, '(a)' ) ''
    end do
  end do

  deallocate ( list )

  return
end
subroutine polyomino_embed_number_test ( )

!*****************************************************************************80
!
!! POLYOMINO_EMBED_NUMBER_TEST tests POLYOMINO_EMBED_NUMBER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) embed_number
  integer ( kind = 4 ) :: mp = 3
  integer ( kind = 4 ) :: mr = 4
  integer ( kind = 4 ) :: np = 2
  integer ( kind = 4 ) :: nr = 4
  integer ( kind = 4 ) :: p(3,2) = reshape ( (/ &
    0, 0, 1, &
    1, 1, 1 /), (/ 3, 2 /) )
  integer ( kind = 4 ) :: r(4,4) = reshape ( (/ &
    0, 1, 1, 1, &
    1, 1, 1, 0, &
    1, 0, 1, 1, &
    1, 1, 1, 1 /), (/ 4, 4 /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_EMBED_NUMBER_TEST:'
  write ( *, '(a)' ) '  POLYOMINO_EMBED_NUMBER reports the number of ways a'
  write ( *, '(a)' ) '  fixed polyomino can be embedded in a region.'

  call polyomino_print ( mr, nr, r, '  The given region R:' )

  call polyomino_print ( mp, np, p, '  The given polyomino P:' )

  call polyomino_embed_number ( mr, nr, r, mp, np, p, embed_number )

  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a)' ) '  As a fixed polyomino, P can be embedded in R in ', embed_number, ' ways.'

  return
end
subroutine polyomino_enumerate_test ( )

!*****************************************************************************80
!
!! POLYOMINO_ENUMERATE_TEST tests the POLYOMINO_ENUMERATE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYOMINO_ENUMERATE_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the POLYOMINO_ENUMERATE library.'

  call polyomino_enumerate_chiral_test ( )
  call polyomino_enumerate_fixed_test ( )
  call polyomino_enumerate_free_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYOMINO_ENUMERATE_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  return
end
subroutine polyomino_enumerate_chiral_test ( )

!*****************************************************************************80
!
!! POLYOMINO_ENUMERATE_CHIRAL_TEST tests POLYOMINO_ENUMERATE_CHIRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n_data
  integer ( kind = 8 ) number
  integer ( kind = 4 ) order

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYOMINO_ENUMERATE_CHIRAL_TEST:'
  write ( *, '(a)' ) '  POLYOMINO_ENUMERATE_CHIRAL returns values of '
  write ( *, '(a)' ) '  the number of chiral polyominoes of given order.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   ORDER         NUMBER'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call polyomino_enumerate_chiral ( n_data, order, number )

    if ( n_data == 0 ) then
      exit
    end if

    write ( *, '(2x,i8,2x,i24)' ) order, number

  end do

  return
end
subroutine polyomino_enumerate_fixed_test ( )

!*****************************************************************************80
!
!! POLYOMINO_ENUMERATE_FIXED_TEST tests POLYOMINO_ENUMERATE_FIXED.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n_data
  integer ( kind = 8 ) number
  integer ( kind = 4 ) order

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYOMINO_ENUMERATE_FIXED_TEST:'
  write ( *, '(a)' ) '  POLYOMINO_ENUMERATE_FIXED returns values of '
  write ( *, '(a)' ) '  the number of fixed polyominoes of given order.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   ORDER         NUMBER'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call polyomino_enumerate_fixed ( n_data, order, number )

    if ( n_data == 0 ) then
      exit
    end if

    write ( *, '(2x,i8,2x,i24)' ) order, number

  end do

  return
end
subroutine polyomino_enumerate_free_test ( )

!*****************************************************************************80
!
!! POLYOMINO_ENUMERATE_FREE_TEST tests POLYOMINO_ENUMERATE_FREE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n_data
  integer ( kind = 8 ) number
  integer ( kind = 4 ) order

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYOMINO_ENUMERATE_FREE_TEST:'
  write ( *, '(a)' ) '  POLYOMINO_ENUMERATE_FREE returns values of '
  write ( *, '(a)' ) '  the number of free polyominoes of given order.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   ORDER         NUMBER'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call polyomino_enumerate_free ( n_data, order, number )

    if ( n_data == 0 ) then
      exit
    end if

    write ( *, '(2x,i8,2x,i24)' ) order, number

  end do

  return
end
subroutine polyomino_index_test ( )

!*****************************************************************************80
!
!! POLYOMINO_INDEX_TEST tests POLYOMINO_INDEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) :: m = 3
  integer ( kind = 4 ) :: n = 4
!
!  P is listed in column-major order
!
  integer ( kind = 4 ) :: p(3*4) = (/ &
    1, 1, 0,&
    0, 1, 1,&
    1, 1, 1,&
    1, 0, 0 /)
  integer ( kind = 4 ) pin(3*4)

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_INDEX_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  POLYOMINO_INDEX assigns an index to each nonzero entry'
  write ( *, '(a)' ) '  of a polyomino.'

  call polyomino_print ( m, n, p, '  The polyomino P:' )

  call polyomino_index ( m, n, p, pin )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  PIN: Index vector for P:'
  write ( *, '(a)' ) ''
  do i = 1, m
    do j = 1, n
      write ( *, '(2x,i2)', advance = 'no' ) pin(i+(j-1)*m)
    end do
    write ( *, '(a)' ) ''
  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_INDEX_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  return
end
subroutine polyomino_lp_write_test ( )

!*****************************************************************************80
!
!! POLYOMINO_LP_WRITE_TEST tests POLYOMINO_LP_WRITE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), allocatable :: a(:,:)
  integer ( kind = 4 ), allocatable :: b(:)
  character ( len = 80 ) filename
  character ( len = 80 ) label
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  call timestamp ( );
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_LP_WRITE_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  POLYOMINO_LP_WRITE writes an LP file associated'
  write ( *, '(a)' ) '  with a binary programming problem for tiling a region'
  write ( *, '(a)' ) '  with copies of a single polyomino.'
!
!  Get the coefficients and right hand side for the Reid system.
!
  call polyomino_monohedral_example_reid_size ( m, n )

  allocate ( a(1:m,1:n) )
  allocate ( b(1:m) )

  call polyomino_monohedral_example_reid_system ( m, n, a, b )
!
!  Create the LP file.
!
  filename = 'reid.lp'
  label = '\ LP file for the Reid example.'

  call polyomino_lp_write ( filename, label, m, n, a, b );

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  POLYOMINO_LP_WRITE created the LP file "' // filename // '"'
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( b )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_LP_WRITE_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  return
end
subroutine polyomino_transform_test ( )

!******************************************************************************/
!
!! POLYOMINO_TRANSFORM_TEST tests POLYOMINO_TRANSFORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Local, integer ( kind = 4 ) M, N, the number of rows and columns in the 
!    representation of the polyomino P.
!
!    Local, integer ( kind = 4 ) P(M*N), a matrix of 0's and 1's representing 
!    the polyomino.  The matrix should be 'tight', that is, there should be a
!    1 in row 1, and in column 1, and in row M, and in column N.
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 3

  character ( len = 80 ) label
  integer ( kind = 4 ) mq
  integer ( kind = 4 ) nq
!
!  P is given by columns, not rows.
!
  integer ( kind = 4 ) :: p(3,3) = reshape ( (/ & 
    0, 1, 0, &
    1, 1, 1, &
    1, 0, 0 /), (/ 3, 3 /) )
  integer q(m*n)
  integer ( kind = 4 ) reflect
  integer ( kind = 4 ) rotate

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_TRANSFORM_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  POLYOMINO_TRANSFORM can transform a polyomino.'
  write ( *, '(a)' ) '  Generate all 8 combinations of rotation and reflection'
  write ( *, '(a)' ) '  applied to a polyomino represented by a binary matrix.'

  call polyomino_print ( m, n, p, '  The given polyomino P:' )

  do reflect = 0, 1
    do rotate = 0, 3

      call polyomino_transform ( m, n, p, rotate, reflect, mq, nq, q )
      write ( label, '(a,i1,a,i1,a)' ) '  P after ', reflect, ' reflections and ', &
        rotate, ' rotations:'

      call polyomino_print ( mq, nq, q, label )

    end do
  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_TRANSFORM_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  return
end
