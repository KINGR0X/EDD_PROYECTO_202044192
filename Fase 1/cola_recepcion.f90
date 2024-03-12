module cola_recepcion
    implicit none
    private

    type, public :: node
        private
        character(:), allocatable :: value
        integer :: img_g, img_p
        type(node), pointer :: next     
    end type node

    type, public :: cola
        private
        type(node), pointer :: head => null()
        type(node), pointer :: end => null()
    contains
        procedure :: append
        procedure :: delete
        procedure :: print
        procedure :: return_cliente_name
        procedure :: return_cliente_img_g
        procedure :: return_cliente_img_p
        procedure :: graficar
    end type cola

contains

    subroutine append(this, value, img_g, img_p)
        class(cola), intent(inout) :: this
        character(len=*), intent(in) :: value
        integer, intent(in) :: img_g, img_p

        type(node), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%img_g = img_g
        temp%img_p = img_p
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
            this%end => temp
        else
            this%end%next => temp
            this%end => temp
        end if

        ! print *, 'Append ', value
        ! print *, 'Append ', img_g
        ! print *, 'Append ', img_p
    end subroutine append

    subroutine delete(this)
        class(cola), intent(inout) :: this
        type(node), pointer :: temp

        if (.not. associated(this%head)) then
            ! print *, 'La cola recepcion esta vacia'
            return
        end if

        print *, this%head%value, ' sale de la cola de recepcion' 
        temp => this%head
        this%head => this%head%next
        deallocate(temp)
    end subroutine delete

    subroutine print(this)
        class(cola), intent(in) :: this
        type(node), pointer :: current

        current => this%head

        print *, '-------- Cola de Recepcion --------'
        print *,  " "

        do while (associated(current))
            print *, current%value
            print *, "img_g: ", current%img_g
            print *, "img_p: ", current%img_p
            print *, "---------------"
            current => current%next
        end do 
    end subroutine print

    function return_cliente_name(this) result(head_value)
        class(cola), intent(in) :: this
        character(len=:), allocatable :: head_value

        if (.not. associated(this%head)) then
            ! print *, 'Cola esta vacia'
            head_value = ""
        else
            head_value = this%head%value
        end if
    end function return_cliente_name

    function return_cliente_img_g(this) result(img_g)
        class(cola), intent(in) :: this
        integer :: img_g

        if (.not. associated(this%head)) then
            ! print *, 'Cola esta vacia'
            img_g = 0
        else
            img_g = this%head%img_g
        end if
    end function return_cliente_img_g

    function return_cliente_img_p(this) result(img_p)
        class(cola), intent(in) :: this
        integer :: img_p

        if (.not. associated(this%head)) then
            ! print *, 'Cola esta vacia'
            img_p = 0
        else
            img_p = this%head%img_p
        end if
    end function  return_cliente_img_p

    subroutine graficar(this, filename)
        class(cola), intent(in) :: this
        character(len=*), intent(in) :: filename
        type(node), pointer :: current
        integer :: img, imp

        integer :: unit
        integer :: count

        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph Cola_recepcion {'
        write(unit, *) 'label= "Cola de Recepcion";'
        write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=gold];' ! Aplicar atributos a todos los nodos
        
        ! Escribir nodos y conexiones
        current => this%head
        count = 0

        do while (associated(current))
            count = count + 1
            img= current%img_g
            imp= current%img_p
            write(unit, *) '    "Node', count, '" [label="', current%value, "\n Img_g =",img,"\n Img_p =",imp,'"];'
            
            if (associated(current%next)) then
                write(unit, *) '    "Node', count, '" -> "Node', count+1, '";'
            end if
            current => current%next
        end do 

        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)
    
        ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpdf ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.pdf')
    
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.pdf'
    end subroutine graficar

end module cola_recepcion

! program main
!     use cola_recepcion
!     implicit none

!     type(cola) :: cola_r
!     character(len=:), allocatable :: nombre_cliente
    
!     call cola_r%append("Juan", 1, 2)
!     call cola_r%append("Pedro", 3, 4)
!     call cola_r%append("Maria", 5, 6)

!     ! call cola_r%print()

!     nombre_cliente = cola_r%return_cliente_name()

!     call cola_r%print()
!     call cola_r%graficar('cola_recepcion')

! end program main