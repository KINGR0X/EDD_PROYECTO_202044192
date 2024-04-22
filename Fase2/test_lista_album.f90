program lista_album_tests
    use lista_album
    implicit none

    type(lista_circular) :: my_list
    integer :: size

    ! Test 1: Append nodes to the list
    call my_list%append(1, "Album 1")
    call my_list%append(2, "Album 2")
    call my_list%append(3, "Album 3")

    ! Test 2: Print the list
    call my_list%print()

    ! Test 3: Append images to the albums
    call my_list%append_img("Album 1", 1)
    call my_list%append_img("Album 1", 2)
    call my_list%append_img("Album 2", 3)
    call my_list%append_img("Album 3", 4)
    call my_list%append_img("Album 3", 5)

    ! Test 4: Print the list with images
    call my_list%print()

    ! Test 5: Get the size of the list
    size = my_list%get_size()
    print *, "List size:", size

    ! Test 6: Get the size of the stack for a specific album
    size = my_list%get_stack_size(1)
    print *, "Stack size for Album 1:", size

    size = my_list%get_stack_size(2)
    print *, "Stack size for Album 2:", size

    size = my_list%get_stack_size(3)
    print *, "Stack size for Album 3:", size

    ! Test 7: Delete a node from the list
    call my_list%delete(2)

    ! Test 8: Print the list after deletion
    call my_list%print()

    ! Test 9: Print the pointers of the list
    call my_list%print_pointers()

    ! Test 10: Graph the list
    call my_list%graficar("album_graph.dot")

end program lista_album_tests