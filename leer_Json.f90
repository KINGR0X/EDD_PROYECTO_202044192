program main
    use json_module
    implicit none

    type(json_file) :: json   ! Variable de tipo json_file
    type(json_value), pointer :: listPointer, personPointer, attributePointer  ! punteros
    type(json_core) :: jsonc  ! Se declara una variable del tipo json_core para acceder a las funciones básicas de JSON
    character(:), allocatable :: nombre  ! Se declara una cadena de caracteres que se asignará dinámicamente
    character(len=100) :: filename

    integer :: i, size       
    logical :: found

    ! El usuario ingresa la direccion del JSON
    print *, "Ingrese la direccion del archivo JSON:"
    read(*, '(A100)') filename

    call json%initialize()    ! Se inicializa el módulo JSON
    call json%load(filename=trim(filename))  !se carga el archivo de entrada Json
    call json%print()         ! Imprimir
    
    call json%info('',n_children=size)

    call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
    call json%get('', listPointer, found)

    do i = 1, size                          ! Se inicia un bucle sobre el número de elementos en el JSON
        call jsonc%get_child(listPointer, i, personPointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
        call jsonc%get_child(personPointer, 'nombre', attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
        
        if (found) then                      ! Si se encuentra el valor asociado con la clave 'nombre'
            call jsonc%get(attributePointer, nombre)  ! Se obtiene el valor y se asigna a la variable 'nombre'
            print *, trim(nombre)           ! Se imprime el nombre sin espacios en blanco adicionales
        end if
    end do

    call json%destroy()                    ! Se finaliza el módulo JSON
end program main