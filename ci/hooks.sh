post_install() {
    link git "${PEARL_PKGDIR}/gitconfig"
    return 0
}

post_update() {
    post_install
}

pre_remove() {
    unlink git "${PEARL_PKGDIR}/gitconfig"
    return 0
}
