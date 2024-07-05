# This file contains unit tests for PassPerms functionality
#
# Author: Josh McIntyre
#
import unittest

import passpermspy

class TestPassPerms(unittest.TestCase):
        
    def test_compute_lengths(self):
       
       perms = passpermspy.compute_lengths(62)
       for i in range(len(passpermspy.LENGTHS)):
            assert perms[i] == 62 ** passpermspy.LENGTHS[i]
            
    def test_compute_complexities(self):
       
       perms = passpermspy.compute_complexities(8)
       for i in range(len(passpermspy.COMPLEXITIES)):
            assert perms[i] == passpermspy.COMPLEXITIES[i] ** 8
       
