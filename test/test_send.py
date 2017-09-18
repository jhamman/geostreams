import os
import unittest
import uuid
from subprocess import Popen

import numpy.testing

from geostreams import client, get_test_exes


def run_project_exe(exe_name):
    """Run exe in ./test directory and raise an error if it doesn't run correctly
    """
    exe_dir = get_test_exes()
    exe = os.path.join(exe_dir, exe_name)
    ret =  Popen([exe], env=os.environ).wait()
    if ret != 0:
        raise RuntimeError(f"{exe_name} returned with code {ret}.")


class ArrayTest(unittest.TestCase):
    def test_read_from_redis(self):
        key = 'A:1'

        run_project_exe('test_send')
        # 200 rows, 100 columns
        array = client.read_from_redis(key)
        x, _ = numpy.mgrid[1:201, 1:101]
        numpy.testing.assert_array_equal(
            x,
            array)

    def test_write_to_redis(self):
        key = str(uuid.uuid4())

        try:
            array = numpy.tile(numpy.arange(100).reshape((100, 1)), (1, 200))
            client.write_to_redis(key, array)
        finally:
            with client.gol_connection() as connection:
                connection.delete(key)

    def test_exes(self):
        for exe in ['test_send',
                    'test_connect',
                    'test_redis_push']:
            print(f"Running {exe}")
            run_project_exe(exe)

    def test_redis_push(self):
        run_project_exe('test_redis_push')

        array = client.read_from_redis("A")
        x, _ = numpy.mgrid[1:201, 1:101] + .5
        numpy.testing.assert_array_equal(
            x,
            array)