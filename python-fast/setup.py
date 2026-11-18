"""
Build the C extension. The C sources are the repository's own C library
(../c/src); they are copied into csrc/ so an sdist is self-contained.
"""
import os
import shutil

from setuptools import Extension, setup

HERE = os.path.dirname(os.path.abspath(__file__))
REPO_C = os.path.join(HERE, "..", "c", "src")
CSRC = os.path.join(HERE, "csrc")

if os.path.isdir(REPO_C):  # building from the repository: refresh the copy
    os.makedirs(CSRC, exist_ok=True)
    for name in ("mpc_designation.c", "mpc_designation.h"):
        shutil.copy2(os.path.join(REPO_C, name), os.path.join(CSRC, name))

setup(
    ext_modules=[
        Extension(
            "mpc_designation_fast._core",
            sources=["src/mpc_designation_fast/_core.c", "csrc/mpc_designation.c"],
            include_dirs=["csrc"],
            define_macros=[("Py_LIMITED_API", "0x03090000")],
            py_limited_api=True,
        )
    ],
    options={"bdist_wheel": {"py_limited_api": "cp39"}},
)
