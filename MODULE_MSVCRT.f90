module MSVCRT
   use ISO_C_BINDING
   implicit none
!   private
!   public fopen,fclose, malloc, free
! Delete fscanf in actual use
!   public fscanf

   interface

     function fopen(filename, mode) &
			bind(C,name='fopen')
         import
         implicit none
         type(C_PTR) fopen
         character(KIND=C_CHAR), intent(IN) :: filename(*)
         character(KIND=C_CHAR), intent(IN) :: mode(*)
      end function fopen

      function fclose(stream) bind(C,name='fclose')
         import
         implicit none
         integer(C_INT) fclose
         type(C_PTR), value :: stream
      end function fclose

! fscanf is only used for the purposes of completing this example
! delete this interface to a variadic function in real use
      function fscanf(stream, format, result) &
			bind(C,name='fscanf')
         import
         implicit none
         integer(C_INT) fscanf
         type(C_PTR),value :: stream
         character(KIND=C_CHAR), intent(IN) :: format(*)
         real(C_DOUBLE) result
      end function fscanf

      function malloc(size) bind(C,name='malloc')
         import
         implicit none
         type(C_PTR) malloc
         integer(C_SIZE_T), value :: size
      end function malloc

      subroutine free(ptr) bind(C,name='free')
         import
         implicit none
         type(C_PTR), value :: ptr
      end subroutine free

   end interface

end module MSVCRT
