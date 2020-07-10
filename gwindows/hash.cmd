CertUtil -hashfile %1 SHA512  >%1.hash.txt
CertUtil -hashfile %1 SHA256 >>%1.hash.txt
CertUtil -hashfile %1 SHA1   >>%1.hash.txt
CertUtil -hashfile %1 MD5    >>%1.hash.txt
