FUNCTION ATOI (str) Result(I)
!
! convert a String to an Integer
!
Integer :: I
Character(LEN=*) :: str

I = 9999
read( str, '(i10)' ) I

End Function