/*
 * _core.c - CPython extension exposing the C library's batch converter.
 *
 * Uses only the stable ABI (Py_LIMITED_API 3.9), so one wheel per platform
 * serves every CPython >= 3.9. NumPy is not needed at build time: the Python
 * layer hands over the raw buffer of a fixed-width 'S' (1 byte/char) or
 * native-endian 'U' (UCS-4, 4 bytes/char) array and wraps the returned
 * buffer with numpy.frombuffer.
 */
#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "mpc_designation.h"

#define OUT_SLOT (MPC_MAX_UNPACKED - 1) /* longest possible result */

/*
 * batch(mode, data, in_chars, char_size) -> (out, status, failures, out_chars)
 *
 *   data       raw array buffer, len == count * in_chars * char_size
 *   char_size  1 for 'S' arrays, 4 for native-endian 'U' arrays
 *   out        bytearray holding count results of out_chars chars each, in
 *              the same char_size; out_chars is the longest result (>= 1)
 *   status     bytearray of count int8 codes (0 = OK)
 *
 * Non-ASCII characters in 'U' input become DEL (0x7F), which the converter
 * rejects, so such elements fail like any other invalid input.
 */
static PyObject *core_batch(PyObject *self, PyObject *args) {
    int mode, char_size;
    PyObject *data_obj;
    char *data;
    Py_ssize_t data_len, in_chars;
    (void)self;

    /* No '#' formats: abi3 wheels built with newer headers would call the
     * non-PY_SSIZE_T_CLEAN PyArg_ParseTuple on older runtimes (3.9-3.12). */
    if (!PyArg_ParseTuple(args, "iOni", &mode, &data_obj, &in_chars, &char_size))
        return NULL;
    if (!PyBytes_Check(data_obj)) {
        PyErr_SetString(PyExc_TypeError, "data must be bytes");
        return NULL;
    }
    if (PyBytes_AsStringAndSize(data_obj, &data, &data_len) < 0)
        return NULL;
    if (mode < MPC_BATCH_CONVERT || mode > MPC_BATCH_UNPACK) {
        PyErr_SetString(PyExc_ValueError, "invalid batch mode");
        return NULL;
    }
    if ((char_size != 1 && char_size != 4) || in_chars <= 0 ||
        data_len % (in_chars * char_size) != 0) {
        PyErr_SetString(PyExc_ValueError, "data length does not match item width");
        return NULL;
    }

    size_t count = (size_t)(data_len / (in_chars * char_size));
    size_t nin = (size_t)in_chars;
    char *narrow = NULL;   /* narrowed copy of 'U' input */
    char *tmp = NULL;      /* fixed OUT_SLOT-wide results */
    PyObject *status = NULL, *out = NULL;
    const char *src = data;
    size_t failures = 0, maxlen = 0;

    status = PyByteArray_FromStringAndSize(NULL, (Py_ssize_t)count);
    if (status == NULL) goto fail;
    if (count > SIZE_MAX / OUT_SLOT) { PyErr_NoMemory(); goto fail; }
    tmp = (char *)malloc(count ? count * OUT_SLOT : 1);
    if (char_size == 4) narrow = (char *)malloc(count ? count * nin : 1);
    if (tmp == NULL || (char_size == 4 && narrow == NULL)) { PyErr_NoMemory(); goto fail; }

    {
        signed char *st = (signed char *)PyByteArray_AsString(status);
        Py_BEGIN_ALLOW_THREADS
        if (char_size == 4) {
            for (size_t i = 0; i < count * nin; i++) {
                uint32_t cp;
                memcpy(&cp, data + 4 * i, 4);
                narrow[i] = (char)(cp < 128 ? cp : 0x7F);
            }
            src = narrow;
        }
        failures = mpc_batch(mode, src, nin, tmp, OUT_SLOT, st, count);
        for (size_t i = 0; i < count; i++) {
            const char *slot = tmp + i * OUT_SLOT;
            size_t len = OUT_SLOT;
            while (len > 0 && slot[len - 1] == '\0') len--;
            if (len > maxlen) maxlen = len;
        }
        Py_END_ALLOW_THREADS
    }

    {
        size_t w = maxlen ? maxlen : 1;
        out = PyByteArray_FromStringAndSize(NULL, (Py_ssize_t)(count * w * (size_t)char_size));
        if (out == NULL) goto fail;
        char *dst = PyByteArray_AsString(out);
        Py_BEGIN_ALLOW_THREADS
        if (char_size == 1) {
            for (size_t i = 0; i < count; i++)
                memcpy(dst + i * w, tmp + i * OUT_SLOT, w);
        } else {
            for (size_t i = 0; i < count; i++) {
                const unsigned char *s = (const unsigned char *)(tmp + i * OUT_SLOT);
                for (size_t j = 0; j < w; j++) {
                    uint32_t cp = s[j];
                    memcpy(dst + 4 * (i * w + j), &cp, 4);
                }
            }
        }
        Py_END_ALLOW_THREADS
        free(tmp);
        free(narrow);
        return Py_BuildValue("(NNnn)", out, status, (Py_ssize_t)failures, (Py_ssize_t)w);
    }

fail:
    free(tmp);
    free(narrow);
    Py_XDECREF(status);
    Py_XDECREF(out);
    return NULL;
}

static PyMethodDef core_methods[] = {
    {"batch", core_batch, METH_VARARGS,
     "batch(mode, data, in_chars, char_size) -> (out, status, failures, out_chars)"},
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef core_module = {
    PyModuleDef_HEAD_INIT, "_core", "MPC designation batch converter (C core).",
    -1, core_methods, NULL, NULL, NULL, NULL
};

PyMODINIT_FUNC PyInit__core(void) {
    PyObject *m = PyModule_Create(&core_module);
    if (m == NULL) return NULL;
    if (PyModule_AddIntConstant(m, "CONVERT", MPC_BATCH_CONVERT) < 0 ||
        PyModule_AddIntConstant(m, "PACK", MPC_BATCH_PACK) < 0 ||
        PyModule_AddIntConstant(m, "UNPACK", MPC_BATCH_UNPACK) < 0) {
        Py_DECREF(m);
        return NULL;
    }
    return m;
}
