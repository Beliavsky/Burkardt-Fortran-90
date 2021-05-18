subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  logical lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end
subroutine high_card_probability ( n, p )

!*****************************************************************************80
!
!! HIGH_CARD_PROBABILITY: winning probabilities for the high card game.
!
!  Discussion:
!
!    The high card game presents the player with a deck of cards, each
!    having an unknown value.  The player is allowed to go throught the
!    deck once, looking at the cards one at a time.  At any time, the player
!    may decide to take a particular card, winning that amount and stopping
!    the game.  If the player continues to the end, by default the last card
!    indicates the amount won.
!
!    An optimal strategy for selecting the highest card is as follows:
!    * look at, but do not select, the first k-1 cards;
!    * stop at the first card, from k to n, that is higher than the first 
!      k-1 cards.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of cards.  
!
!    Output, real ( kind = 8 ) P(N).  P(K) is the probability that a strategy 
!    that skips K-1 cards will win, given that the deck has N cards.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) t

  do i = 1, n
    t = 0.0D+00
    do j = i, n - 1
      t = t + 1.0D+00 / real ( j, kind = 8 )
    end do
    p(i) = ( 1.0D+00 + real ( i - 1, kind = 8 ) * t ) / real ( n, kind = 8 )
  end do

  return
end
subroutine high_card_shuffle ( n, seed, sequence )

!*****************************************************************************80
!
!! HIGH_CARD_SHUFFLE generates a sequence of numeric "cards" for a game.
!
!  Discussion:
!
!    In this game, you know that the deck contains N cards.  You win by 
!    choosing the highest card in the deck.  You don't know what this card
!    is, and you must choose your card by saying "stop" as, one by one,
!    the cards of the deck are exposed.  
!
!    A random guesser would get the high card with probability 1/N.
!
!    An intelligent guesser can do much better.
!
!    It is the goal of this program so "shuffle" a deck of cards suitable
!    for this game.  The problem is that we know the highest card in an
!    ordinary deck.  Let's replace the cards by integers.  Then if we know
!    in advance the range of the cards (say, they must lie between 1 and 
!    1,000), it may be true that we can guess the card that is the maximum.
!
!    However, this program produces a sequence of integer card values for
!    which no information can be gained from the values.  It does this
!    by regarding the card values as binary integers between 1 and 2^N - 1.
!    We can make a perfectly information-free sequence as follows:
!
!      Card 1 sets bit N-1 to 1.
!      Card 2 sets bit N-2 to 1, bit  N-1 randomly.
!      ...
!      Card I sets bit N-I to 1, bits N-1 down to N-I+1 randomly.
!      ...
!      Card N sets bit N-N to 1, bits N-1 down to 1 randomly.
!
!    The I-th card has equal probability to fall in any of the I intervals
!    defined by the I-1 previous cards.  So, knowing the previous cards tells
!    you absolutely nothing about where the next card will fall, and each
!    card is, at the moment you see it, as good a guess for the maximum as
!    the unseen cards.
!
!    For example, the command "high_card_shuffle(7)" might yield
!
!      64    96    80     8     4    82    29
!    or
!      64    32    16    24    12    58    73
!    or
!      64    96    48     8   100    26    93
!    or
!      64    96    16    56    76   122    71
!
!    in which the highest card is #2, #7, #5, or #6 respectively.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of cards.  N probably needs to 
!    be less than 32.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) SEQUENCE(N), a set of N integer values 
!    that can be used as the cards in the high card guessing game.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) sequence(n)

  if ( 32 <= n ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HIGH_CARD_SHUFFLE - Fatal error!'
    write ( *, '(a)' ) '  This program can only handle N < 32.'
    stop 1
  end if

  do i = 1, n
    c = 2 ** ( n - i )
    do j = 1, i - 1
      k = i4_uniform_ab ( 0, 1, seed )
      c = c + k * 2 ** ( n - i + j )
    end do
    sequence(i) = c
  end do

  return
end
subroutine high_card_simulation ( deck_size, skip_num, trial_num, seed, p )

!*****************************************************************************80
!
!! HIGH_CARD_SIMULATION simulates a game of choosing the highest card in a deck.
!
!  Discussion:
!
!    You are given a deck of DECK_SIZE cards.
!
!    Your goal is to select the high card.  For convenience, we can assume 
!    the cards are a permutation of the integers from 1 to DECK_SIZE, but in
!    fact the user mustn't see such values or else it's obvious which is the
!    largest card.
!
!    However, your choice is made under the following rules:  You may turn over
!    one card at a time.  When a card is turned over, you may declare that to be
!    your choice, or else turn over another card.  If you have not chosen a card
!    by the end, then your choice is the final card.
!
!    If you have no idea what to do, and simply decide in advance to pick
!    a card "at random", that is, for example, you decide to pick the 15th card
!    before having seen any cards, then your probability of winning is 
!    1/DECK_SIZE.
!
!    The question is, can you do better than that?
!
!    Your strategy is as follows: always look at the first SKIP_NUM cards 
!    without choosing them.  Then choose the very next card you encounter 
!    that is larger than the cards you skipped.
!
!    Using this program, you can easily see that skipping 5 cards is much better
!    than picking one at random, skipping 10 is even better, and so on.  
!    Of course, you can't skip too many cards, and in fact, the results seem 
!    to be best for somewhere around 30 to 35 cards skipped.  For problems 
!    like this, the optimal value is somewhere around 1 / e, where E is the 
!    base of the natural logarithm system.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DECK_SIZE, the number of cards in the deck.
!    2 <= DECK_SIZE.  Default value is 52;
!
!    Input, integer ( kind = 4 ) SKIP_NUM, the number of initial cards you plan 
!    to examine but will NOT select.  If SKIP_NUM is 0, you don't look at any 
!    cards first.  0 <= SKIP_NUM < DECK_SIZE.
!
!    Input, integer ( kind = 4 ) TRIAL_NUM, the number of times we will 
!    simulate this process.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, real ( kind = 8 ) P, the estimated probability that your strategy 
!    of skipping SKIP_NUM cards and then selecting the next card that is 
!    bigger, will result in choosing the highest card.
!
  implicit none

  integer ( kind = 4 ) deck_size

  integer ( kind = 4 ) card
  integer ( kind = 4 ) cards(deck_size)
  integer ( kind = 4 ) choice
  integer ( kind = 4 ) correct
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  real ( kind = 8 ) p
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) skip_max
  integer ( kind = 4 ) skip_num
  integer ( kind = 4 ) trial
  integer ( kind = 4 ) trial_num
  integer ( kind = 4 ) true_max
!
!  Check values.
!
  if ( deck_size < 2 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'HIGH_CARD_SIMULATION - Fatal error!'
    write ( *, '(a)' ) '  DECK_SIZE must be at least 2.'
    write ( *, '(a,i6)' ) '  Your value was ', deck_size
    stop 1
  end if

  if ( skip_num < 0 ) then
    skip_num = 0
  end if

  if ( deck_size <= skip_num ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'HIGH_CARD_SIMULATION - Fatal error!'
    write ( *, '(a)' ) '  SKIP_NUM must be less than DECK_SIZE.'
    write ( *, '(a,i6)' ) '  Your DECK_SIZE = ', deck_size
    write ( *, '(a,i6)' ) '  Your SKIP_NUM = ', skip_num
    stop 1
  end if

  if ( trial_num < 1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'HIGH_CARD_SIMULATION - Fatal error!'
    write ( *, '(a)' ) '  TRIAL_NUM must be at least 1.'
    write ( *, '(a,i6)' ) '  Your TRIAL_NUM was ', trial_num 
    stop 1
  end if

  correct = 0

  do trial = 1, trial_num

    call permutation_random ( deck_size, seed, cards )

    if ( 1 <= skip_num ) then
      skip_max = maxval ( cards(1:skip_num) )
    else
      skip_max = - i4_huge
    end if

    true_max = maxval ( cards(1:deck_size) )
!
!  In case you don't encounter a card larger than SKIP_MAX,
!  we'll assume you pick the last card in the deck, even though
!  you know it's a loser.
!
    choice = cards(deck_size)
!
!  Turn over the remaining cards in the deck, but stop
!  immediately when you find one bigger than SKIP_MAX.
!
    do card = skip_num + 1, deck_size
      if ( skip_max < cards(card) ) then
        choice = cards(card)
        exit
      end if
    end do
!
!  Record successful choices.
!
    if ( choice == true_max ) then
      correct = correct + 1
    end if

  end do
!
!  Estimate the probability.
!
  p = real ( correct, kind = 8 ) / real ( trial_num, kind = 8 )

  return
end
function i4_uniform_ab ( a, b, seed )

!*****************************************************************************80
!
!! I4_UNIFORM_AB returns a scaled pseudorandom I4 between A and B.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ) value.
!
!    The pseudorandom number will be scaled to be uniformly distributed
!    between A and B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, the limits of the interval.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, integer ( kind = 4 ) I4_UNIFORM_AB, a number between A and B.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) k
  real ( kind = 4 ) r
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) value

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r = real ( seed, kind = 4 ) * 4.656612875E-10
!
!  Scale R to lie between A-0.5 and B+0.5.
!
  r = ( 1.0E+00 - r ) * ( real ( min ( a, b ), kind = 4 ) - 0.5E+00 ) & 
    +             r   * ( real ( max ( a, b ), kind = 4 ) + 0.5E+00 )
!
!  Use rounding to convert R to an integer between A and B.
!
  value = nint ( r, kind = 4 )

  value = max ( value, min ( a, b ) )
  value = min ( value, max ( a, b ) )

  i4_uniform_ab = value

  return
end
subroutine i4vec_print ( n, a, title )

!*****************************************************************************80
!
!! I4VEC_PRINT prints an I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, integer ( kind = 4 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,a,2x,i12)' ) i, ':', a(i)
  end do

  return
end
subroutine permutation_random ( n, seed, p )

!*****************************************************************************80
!
!! PERMUTATION_RANDOM returns a random permutation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of objects to permute.
!
!    Input, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, integer ( kind = 4 ) P(N), a permutation of the integers 
!    from 1 to N.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) k
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) p1
  integer ( kind = 4 ) seed

  do i = 1, n
    p(i) = i
  end do

  do i = 1, n - 1

    k = i4_uniform_ab ( i, n, seed )

    p1   = p(i)
    p(i) = p(k)
    p(k) = p1

  end do

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

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
