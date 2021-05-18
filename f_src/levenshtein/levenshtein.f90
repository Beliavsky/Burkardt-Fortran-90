subroutine levenshtein_distance ( m, s, n, t, distance )

!*****************************************************************************80
!
!! LEVENSHTEIN_DISTANCE computes the Levenshtein distance between strings.
!
!  Discussion:
!
!    Let S and T be source and target strings.  Consider the task of
!    converting S to T in the minimal number of steps, involving
!    * Insertion: insert a new character
!    * Deletion: delete a character
!    * Substitution: swap one character for another.
!    The Levenshtein distance from S to T is the minimal number of such
!    steps required to transform S into T.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the length of string S.
!
!    Input, character ( len = * ) S, the first string.
!
!    Input, integer ( kind = 4 ) N, the length of string T.
!
!    Input, character ( len = * ) T, the second string.
!
!    Output, integer ( kind = 4 ) DISTANCE, the Levenshtein distance between the
!    two strings.
!
  implicit none

  integer ( kind = 4 ), allocatable :: d(:,:)
  integer ( kind = 4 ) distance
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character ( len = * ) s
  integer ( kind = 4 ) substitution_cost
  character ( len = * ) t

  allocate ( d(0:m,0:n) )

  d(0,0) = 0
!
!  Source prefixes can be transformed into empty string by
!  dropping all characters,
!
  do i = 1, m
    d(i,0) = i
  end do
!
!  Target prefixes can be reached from empty source prefix
!  by inserting every character.
!
  do j = 1, n
    d(0,j) = j
  end do

  do j = 1, n
    do i = 1, m
      if ( s(i:i) == t(j:j) ) then
        substitution_cost = 0
      else
        substitution_cost = 1
      end if
      d(i,j) = min ( d(i-1,j) + 1, &
               min ( d(i,j-1) + 1, &
                     d(i-1,j-1) + substitution_cost ) )
    end do
  end do
 
  distance = d(m,n)

  deallocate ( d )

  return
end
subroutine levenshtein_distance_test ( )

!*****************************************************************************80
!
!! LEVENSHTEIN_DISTANCE_TEST tests LEVENSHTEIN_DISTANCE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d1
  integer ( kind = 4 ) d2
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character ( len = 5 ) :: s1 = 'water'
  character ( len = 6 ) :: s2 = 'kitten'
  character ( len = 8 ) :: s3 = 'saturday'
  character ( len = 10 ) :: s4 = 'pheromones'
  character ( len = 4 ) :: t1 = 'wine'
  character ( len = 7 ) :: t2 = 'sitting'
  character ( len = 6 ) :: t3 = 'sunday'
  character ( len = 12 ) :: t4 = 'photographer'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEVENSHTEIN_DISTANCE_TEST:'
  write ( *, '(a)' ) '  LEVENSHTEIN_DISTANCE computes the Levenshtein distance'
  write ( *, '(a)' ) '  between two strings.'

  m = len ( s1 )
  n = len ( t1 )
  call levenshtein_distance ( m, s1, n, t1, d1 )
  d2 = 3
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  S = "' // s1 // '"'
  write ( *, '(a)' ) '  T = "' // t1 // '"'
  write ( *, '(a,i4)' ) '  Computed distance = ', d1
  write ( *, '(a,i4)' ) '  Correct distance  = ', d2

  m = len ( s2 )
  n = len ( t2 )
  call levenshtein_distance ( m, s2, n, t2, d1 )
  d2 = 3
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  S = "' // s2 // '"'
  write ( *, '(a)' ) '  T = "' // t2 // '"'
  write ( *, '(a,i4)' ) '  Computed distance = ', d1
  write ( *, '(a,i4)' ) '  Correct distance  = ', d2

  m = len ( s3 )
  n = len ( t3 )
  call levenshtein_distance ( m, s3, n, t3, d1 )
  d2 = 3
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  S = "' // s3 // '"'
  write ( *, '(a)' ) '  T = "' // t3 // '"'
  write ( *, '(a,i4)' ) '  Computed distance = ', d1
  write ( *, '(a,i4)' ) '  Correct distance  = ', d2

  m = len ( s4 )
  n = len ( t4 )
  call levenshtein_distance ( m, s4, n, t4, d1 )
  d2 = 8
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  S = "' // s4 // '"'
  write ( *, '(a)' ) '  T = "' // t4 // '"'
  write ( *, '(a,i4)' ) '  Computed distance = ', d1
  write ( *, '(a,i4)' ) '  Correct distance  = ', d2

  return
end
subroutine levenshtein_matrix ( m, s, n, t, d )

!*****************************************************************************80
!
!! LEVENSHTEIN_MATRIX computes the Levenshtein distance matrix between strings.
!
!  Discussion:
!
!    Let S and T be source and target strings.  Consider the task of
!    converting S to T in the minimal number of steps, involving
!    * Insertion: insert a new character
!    * Deletion: delete a character
!    * Substitution: swap one character for another.
!    The Levenshtein distance from S to T is the minimal number of such
!    steps required to transform S into T.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the length of string S.
!
!    Input, character ( len = * ) S, the first string.
!
!    Input, integer ( kind = 4 ) N, the length of string T.
!
!    Input, character ( len = * ) T, the second string.
!
!    Output, integer ( kind = 4 ) D(0:M,0:N), the Levenshtein matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) d(0:m,0:n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  character ( len = * ) s
  integer ( kind = 4 ) substitution_cost
  character ( len = * ) t

  d(0,0) = 0
!
!  Source prefixes can be transformed into empty string by
!  dropping all characters,
!
  do i = 1, m
    d(i,0) = i
  end do
!
!  Target prefixes can be reached from empty source prefix
!  by inserting every character.
!
  do j = 1, n
    d(0,j) = j
  end do

  do j = 1, n
    do i = 1, m
      if ( s(i:i) == t(j:j) ) then
        substitution_cost = 0
      else
        substitution_cost = 1
      end if
      d(i,j) = min ( d(i-1,j) + 1, &
               min ( d(i,j-1) + 1, &
                     d(i-1,j-1) + substitution_cost ) )
    end do
  end do
 
  return
end
subroutine levenshtein_matrix_test ( )

!*****************************************************************************80
!
!! LEVENSHTEIN_MATRIX_TEST tests LEVENSHTEIN_MATRIX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), allocatable :: d(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character ( len = 5 ) :: s1 = 'water'
  character ( len = 6 ) :: s2 = 'kitten'
  character ( len = 8 ) :: s3 = 'saturday'
  character ( len = 10 ) :: s4 = 'pheromones'
  character ( len = 4 ) :: t1 = 'wine'
  character ( len = 7 ) :: t2 = 'sitting'
  character ( len = 6 ) :: t3 = 'sunday'
  character ( len = 12 ) :: t4 = 'photographer'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEVENSHTEIN_MATRIX_TEST:'
  write ( *, '(a)' ) '  LEVENSHTEIN_MATRIX computes the Levenshtein matrix'
  write ( *, '(a)' ) '  associated with the computation of the Levenshtein'
  write ( *, '(a)' ) '  distance between two strings.'

  m = len ( s1 )
  n = len ( t1 )
  allocate ( d(0:m,0:n) )
  call levenshtein_matrix ( m, s1, n, t1, d )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  S = "' // s1 // '"'
  write ( *, '(a)' ) '  T = "' // t1 // '"'
  do i = 0, m
    do j = 0, n
      write ( *, '(1x,i2)', advance='no' ) d(i,j)
    end do
    write ( *, '(a)' ) ''
  end do
  deallocate ( d )

  m = len ( s2 )
  n = len ( t2 )
  allocate ( d(0:m,0:n) )
  call levenshtein_matrix ( m, s2, n, t2, d )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  S = "' // s2 // '"'
  write ( *, '(a)' ) '  T = "' // t2 // '"'
  do i = 0, m
    do j = 0, n
      write ( *, '(1x,i2)', advance='no' ) d(i,j)
    end do
    write ( *, '(a)' ) ''
  end do
  deallocate ( d )

  m = len ( s3 )
  n = len ( t3 )
  allocate ( d(0:m,0:n) )
  call levenshtein_matrix ( m, s3, n, t3, d )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  S = "' // s3 // '"'
  write ( *, '(a)' ) '  T = "' // t3 // '"'
  do i = 0, m
    do j = 0, n
      write ( *, '(1x,i2)', advance='no' ) d(i,j)
    end do
    write ( *, '(a)' ) ''
  end do
  deallocate ( d )

  m = len ( s4 )
  n = len ( t4 )
  allocate ( d(0:m,0:n) )
  call levenshtein_matrix ( m, s4, n, t4, d )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  S = "' // s4 // '"'
  write ( *, '(a)' ) '  T = "' // t4 // '"'
  do i = 0, m
    do j = 0, n
      write ( *, '(1x,i2)', advance='no' ) d(i,j)
    end do
    write ( *, '(a)' ) ''
  end do
  deallocate ( d )

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

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
