module matrix_m
    implicit none
    private

    type :: node_val
        private
        logical :: exists = .false.
        character(len=:), allocatable :: value
    end type node_val

    type :: node
        private
        integer :: i, j
        character(len=:), allocatable :: value
        type(node), pointer :: up => null()
        type(node), pointer :: down => null()
        type(node), pointer :: right => null()
        type(node), pointer :: left => null()
    end type node

    type, public :: matrix
        private
        type(node), pointer :: root
        integer :: width = 0
        integer :: height = 0
    contains
        procedure :: insert
        procedure :: insertRowHeader
        procedure :: insertColumnHeader
        procedure :: insertInRow
        procedure :: insertInColumn
        procedure :: searchRow
        procedure :: searchColumn
        procedure :: nodeExists
        procedure :: print
        procedure :: printColumnHeaders
        procedure :: getValue
        procedure :: graficar
    end type

contains

    subroutine graficar(self,filename)
        class(matrix), intent(inout) :: self  
        character(len=*), intent(in) :: filename
        integer :: fila,f
        integer :: colum,c
        type(node), pointer :: aux
        type(node_val) :: val
        integer :: unit
        

        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph Matriz {'
        write(unit, *) '    node [shape=box, style=filled];' ! Aplicar atributos a todos los nodos

        aux => self%root%down

        ! Todo se grafica fila por fila -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

        ! Se imprimen los encabezados de las columnas
        do fila=-1, self%width
            if (fila == -1) then
                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0, '" [label="root"]'

                ! Union con el primer nodo de la fila
                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0,'" -> "Node_fila_-1_0','"' 
                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0,'" -> "Node_fila_-1_0','"[dir=back]' 
                
            else
                write(unit, '(*(g0))') '"Node_encabezado_', fila-1, '_', 0,'" -> "Node_encabezado_', fila, '_', 0, '"'
                write(unit, '(*(g0))') '"Node_encabezado_', fila-1, '_', 0,'" -> "Node_encabezado_', fila, '_', 0, '"[dir=back]' 

                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0, '" [label="',fila,'"]'

                ! Union de encabezados con los nodos de la fila
                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0,'" -> "Node', 0,'_',fila,'"'
                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0,'" -> "Node', 0,'_',fila,'"[dir=back]'



            end if
        end do

        ! Se establecen que nodos conforman una fila
        write(unit, '(*(g0))') ' '
        write(unit, '(*(g0))', advance="no") '{rank=same;'
        do fila=-1, self%width
            write(unit, '(*(g0))',advance="no") '"Node_encabezado_', fila, '_', 0, '";'
        end do
        write(unit, '(*(g0))',advance="yes") '};'

        ! salto de linea para que este tan pegado
        write(unit, '(*(g0))') ' '


        ! Se inicializa j
        fila=0
        f=0

        do fila = 0, self%height
            f=fila
            colum = 0
            ! write(*, fmt='(I3)', advance='no') i

            ! Inicio de la fila
            write(unit, '(*(g0))') '"Node_fila_', fila-1,"_",colum,'" [label="',fila,'"]'

            !Union de las filas 
            write(unit, '(*(g0))') '"Node_fila_', fila-1,"_",colum,'" -> "Node', fila,'_',colum,'"'
            write(unit, '(*(g0))') '"Node_fila_', fila-1,"_",colum,'" -> "Node', fila,'_',colum,'"[dir=back]'

            !Union con flechas a la derecha
            if(.not. fila== self%height) then
                write(unit, '(*(g0))') '"Node_fila_', fila-1,"_",colum,'" -> "Node_fila_', fila,'_',colum,'"'
                write(unit, '(*(g0))') '"Node_fila_', fila-1,"_",colum,'" -> "Node_fila_', fila,'_',colum,'"[dir=back]'
            end if

            
            do colum = 0, self%width
                val = self%getValue(fila,colum)

                if(.not. val%exists) then
                    ! write(*, fmt='(I3)', advance='no') 0
                    write(unit, '(*(g0))') '"Node', fila,"_",colum,'" [label="',colum,'", group=', fila,']'
                    write(unit, '(*(g0))') '"Node', fila,"_",colum,'" [label="", group=', fila,']'

                else
                    ! write(*, fmt='(L3)', advance='no') val%value

                    write(unit, '(*(g0))') '"Node', fila,"_",colum,'" [label= " ", fillcolor="',val%value,'", group=', fila,']'

                end if

                if (.not. colum==0) then
                    !Union de las filas izquierda y derecha
                    write(unit, '(*(g0))') '"Node', fila,"_",colum-1,'" -> "Node', fila,'_',colum,'"'
                    write(unit, '(*(g0))') '"Node', fila,"_",colum-1,'" -> "Node', fila,'_',colum,'"[dir=back]'
                end if

                ! Union arriba y abajo
                if (.not. colum==0 ) then

                    if (.not. fila==self%height) then
                    ! Union arriba y abajo
                        write(unit, '(*(g0))') '"Node', fila,"_",colum-1,'" -> "Node', fila+1,'_',colum-1,'"'
                        write(unit, '(*(g0))') '"Node', fila,"_",colum-1,'" -> "Node', fila+1,'_',colum-1,'"[dir=back]'
                    end if
                end if

                ! Union arriba y abajo del ultimo nodo
                if (colum==self%width) then
                    if (.not. fila==self%height) then
                        write(unit, '(*(g0))') '"Node', fila,"_",colum,'" -> "Node', fila+1,'_',colum,'"'
                        write(unit, '(*(g0))') '"Node', fila,"_",colum,'" -> "Node', fila+1,'_',colum,'"[dir=back]'
                    end if
                end if

            end do

            ! === Se establecen que nodos conforman una fila ===

            ! salto de linea para que este tan pegado
            write(unit, '(*(g0))') ' '
            write(unit, '(*(g0))', advance="no") '{rank=same;'
            c=0
            do c=-1, self%width
                if (c==-1)then 
                    write(unit, '(*(g0))',advance="no") '"Node_fila_', f-1, '_', c+1, '";'
                else
                    write(unit, '(*(g0))',advance="no") '"Node', f,"_",c,'";'
                end if
            end do
            write(unit, '(*(g0))',advance="yes") '};'

            write(unit, '(*(g0))') ' '

            ! {rank=same;"Node_fila_-1_0";"Node0_0","Node0_1","Node0_2","Node0_3","Node0_4","Node0_5","Node0_6","Node0_7","Node0_8","Node0_9","Node0_10";};

        end do


        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)
    
        ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
    
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'


    end subroutine graficar



    subroutine insert(self, i, j, value) 
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: i
        integer, intent(in) :: j
        character(len=*), intent(in) :: value

        type(node), pointer :: new
        type(node), pointer :: row
        type(node), pointer :: column

        allocate(new)
        new = node(i=i, j=j, value=value)

        if(.not. associated(self%root)) then
            allocate(self%root)
            self%root = node(i=-1, j=-1)
        end if

        row => self%searchRow(i)
        column => self%searchColumn(j)

        if(j > self%width) self%width = j
        if(i > self%height) self%height = i

        if(.not. self%nodeExists(new)) then
            if(.not. associated(column)) then
                column => self%insertColumnHeader(j)
            end if

            if(.not. associated(row)) then
                row => self%insertRowHeader(i)
            end if
            call self%insertInColumn(new, row)
            call self%insertInRow(new, column)
        end if
    end subroutine insert

    function searchColumn(self, j) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: j

        type(node), pointer :: actual
        actual => self%root

        do while(associated(actual))
            if(actual%j == j) return
            actual => actual%right
        end do
    end function searchColumn

    function searchRow(self, i) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i

        type(node), pointer :: actual
        actual => self%root

        do while(associated(actual))
            if(actual%i == i) return
            actual => actual%down
        end do
    end function searchRow

    function nodeExists(self, new) result(exists)
        class(matrix), intent(inout) :: self  
        type(node), pointer :: new
        
        logical :: exists
        type(node), pointer :: rowHeader
        type(node), pointer :: column
        rowHeader => self%root
        exists = .false.

        do while(associated(rowHeader))
            if(rowHeader%i == new%i) then
                column => rowHeader
                do while(associated(column))
                    if(column%j == new%j) then
                        column%value = new%value
                        exists = .true.
                        return
                    end if
                    column => column%right
                end do
                return
            end if
            rowHeader => rowHeader%down
        end do
        return
    end function nodeExists

    function insertRowHeader(self, i) result(newRowHeader)
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: i

        type(node), pointer :: newRowHeader
        allocate(newRowHeader)

        newRowHeader = node(i=i, j=-1)
        call self%insertInRow(newRowHeader, self%root)
    end function insertRowHeader

    subroutine insertInRow(self, new, rowHeader)
        class(matrix), intent(inout) :: self
        type(node), pointer :: new
        type(node), pointer :: rowHeader

        type(node), pointer :: actual
        actual => rowHeader

        do while(associated(actual%down))
            if(new%i < actual%down%i .and. new%i > actual%i) then
                new%down => actual%down
                new%up => actual
                actual%down%up => new
                actual%down => new
                exit
            end if
            actual => actual%down
        end do

        if(.not. associated(actual%down)) then
            actual%down => new
            new%up => actual
        end if
    end subroutine insertInRow

    function insertColumnHeader(self, j) result(newColumnHeader)
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: j

        type(node), pointer :: newColumnHeader
        allocate(newColumnHeader)

        newColumnHeader = node(i=-1, j=j)
        call self%insertInColumn(newColumnHeader, self%root)
    end function insertColumnHeader

    subroutine insertInColumn(self, new, columnHeader)
        class(matrix), intent(inout) :: self
        type(node), pointer :: new
        type(node), pointer :: columnHeader
        
        type(node), pointer :: actual
        actual => columnHeader
        do while(associated(actual%right))
            if(new%j < actual%right%j .and. new%j > actual%j) then
                new%right => actual%right
                new%left => actual
                actual%right%left => new
                actual%right => new
                exit
            end if
            actual => actual%right
        end do
        
        if(.not. associated(actual%right)) then
            actual%right => new
            new%left => actual
        end if
    end subroutine insertInColumn

    subroutine print(self)
        class(matrix), intent(inout) :: self  
        integer :: i
        integer :: j
        type(node), pointer :: aux
        type(node_val) :: val
        aux => self%root%down

        call self%printColumnHeaders()

        do i = 0, self%height
            print *, ""
            write(*, fmt='(I3)', advance='no') i
            do j = 0, self%width
                val = self%getValue(i,j)
                if(.not. val%exists) then
                    write(*, fmt='(I3)', advance='no') 0
                else
                    write(*, fmt='(L3)', advance='no') val%value

                    !write(*, fmt='(A)', advance='no') val%value

                end if
            end do
        end do
    end subroutine print

    subroutine printColumnHeaders(self)
        class(matrix), intent(in) :: self
        integer :: j

        do j=-1, self%width
            write(*, fmt='(I3)', advance='no') j
        end do
    end subroutine printColumnHeaders

    function getValue(self, i, j) result(val)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j
        
        type(node), pointer :: rowHeader
        type(node), pointer :: column
        type(node_val) :: val
        rowHeader => self%root

        do while(associated(rowHeader))
            if(rowHeader%i == i) then
                column => rowHeader
                do while(associated(column))
                    if(column%j == j) then
                        val%value = column%value
                        val%exists = .true.
                        return
                    end if
                    column => column%right
                end do
                return
            end if
            rowHeader => rowHeader%down
        end do
    end function getValue
end module matrix_m

! program main
!     use matrix_m
!     implicit none
    
!     type(matrix) :: m
!     !             i, j
!     !            fila, columna
!     call m%insert(1, 5, "#000001")
!     call m%insert(0, 3, "#FF0000")  
!     call m%insert(1, 1, "#000000")
!     call m%insert(4, 1, "#000041")
!     call m%insert(5, 3, "#000000")
!     call m%insert(5, 10, "#000000")
!     call m%insert(1, 1, "#000000")
!     call m%insert(3, 1, "#000000")


!     call m%print()

!     call m%graficar("capas.dot")



! end program main