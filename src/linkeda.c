#include <stddef.h>
#ifndef _WIN32
#include <unistd.h>
#include <fcntl.h>
#endif

#include <R.h>
#include <Rinternals.h>
#ifndef _WIN32
#include <R_ext/eventloop.h>
#endif
#include <R_ext/Visibility.h>
#include <R_ext/Rdynload.h>

#ifndef _WIN32
static int rls_task_fd = -1;
static InputHandler *rls_task_handler = NULL;
static int rls_task_handler_busy = 0;

static void rls_unregister_task_handler_impl(void)
{
    if (rls_task_handler != NULL) {
        removeInputHandler(&R_InputHandlers, rls_task_handler);
        rls_task_handler = NULL;
    }
    if (rls_task_fd >= 0) {
        close(rls_task_fd);
        rls_task_fd = -1;
    }
}

static void rls_task_input_handler(void *userData)
{
    (void)userData;
    if (rls_task_fd < 0) {
        return;
    }

    if (rls_task_handler_busy) {
        return;
    }

    char buffer[256];
    while (read(rls_task_fd, buffer, sizeof(buffer)) > 0) {
    }

    rls_task_handler_busy = 1;

    int error = 0;
    SEXP package = PROTECT(mkString("LinkEDA"));
    SEXP namespace = PROTECT(R_FindNamespace(package));
    SEXP call = PROTECT(lang1(install(".rls_process_backend_tasks")));
    R_tryEval(call, namespace, &error);
    UNPROTECT(3);

    rls_task_handler_busy = 0;
}

static SEXP rls_register_task_handler(SEXP path)
{
    if (!Rf_isString(path) || XLENGTH(path) != 1 || STRING_ELT(path, 0) == NA_STRING) {
        Rf_error("`path` must be a single non-missing string.");
    }

    rls_unregister_task_handler_impl();

    const char *fifo_path = CHAR(STRING_ELT(path, 0));
    /* Keep one writer endpoint open in this process.  A FIFO opened read-only
       becomes permanently readable (EOF) whenever the native backend closes
       its short-lived notification writer.  R's event loop would then invoke
       this handler continuously and starve normal console input. */
    rls_task_fd = open(fifo_path, O_RDWR | O_NONBLOCK);
    if (rls_task_fd < 0) {
        return ScalarLogical(FALSE);
    }
    int descriptor_flags = fcntl(rls_task_fd, F_GETFD);
    if (descriptor_flags >= 0) {
        (void)fcntl(rls_task_fd, F_SETFD, descriptor_flags | FD_CLOEXEC);
    }

    R_InputHandlers = addInputHandler(R_InputHandlers, rls_task_fd, rls_task_input_handler, XActivity);
    rls_task_handler = getInputHandler(R_InputHandlers, rls_task_fd);
    if (rls_task_handler == NULL) {
        close(rls_task_fd);
        rls_task_fd = -1;
        return ScalarLogical(FALSE);
    }

    return ScalarLogical(TRUE);
}
#else
static SEXP rls_register_task_handler(SEXP path)
{
    (void)path;
    return ScalarLogical(FALSE);
}
#endif

static SEXP rls_unregister_task_handler(SEXP unused)
{
    (void)unused;
#ifndef _WIN32
    rls_unregister_task_handler_impl();
#endif
    return ScalarLogical(TRUE);
}

static SEXP rls_fifo_has_reader(SEXP path)
{
#ifndef _WIN32
    if (!Rf_isString(path) || XLENGTH(path) != 1 || STRING_ELT(path, 0) == NA_STRING) {
        Rf_error("`path` must be a single non-missing string.");
    }

    const char *fifo_path = CHAR(STRING_ELT(path, 0));
    int fd = open(fifo_path, O_WRONLY | O_NONBLOCK);
    if (fd < 0) {
        return ScalarLogical(FALSE);
    }
    close(fd);
    return ScalarLogical(TRUE);
#else
    (void)path;
    return ScalarLogical(FALSE);
#endif
}

static const R_CallMethodDef callMethods[] = {
    {"rls_register_task_handler", (DL_FUNC) &rls_register_task_handler, 1},
    {"rls_unregister_task_handler", (DL_FUNC) &rls_unregister_task_handler, 1},
    {"rls_fifo_has_reader", (DL_FUNC) &rls_fifo_has_reader, 1},
    {NULL, NULL, 0}
};

void attribute_visible R_init_LinkEDA(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

void attribute_visible R_unload_LinkEDA(DllInfo *dll)
{
    (void)dll;
#ifndef _WIN32
    rls_unregister_task_handler_impl();
#endif
}
