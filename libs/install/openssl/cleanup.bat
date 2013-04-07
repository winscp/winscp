@echo off

cd crypto
del /s Makefile.*
del /s README
del /s VERSION
del /s INSTALL
del /s todo
del /s test
del /s example
del /s generate

del /s *.pl
del /s *.mul
del /s *.pem
del /s *.ec

del /s *586*.*
del /s *ia64*.*
del /s *x86_64*.*
del /s *mips*.*
del /s *risc2*.*
del /s *sparc*.*
del /s *vms*.*

del /s *speed*.c
del /s *test*.c


rmdir /s /q aes\asm
rem del asn1\p8_key.c
rem del asn1\tasn_prn.c
del bf\bf_cbc.c
del bf\bf_enc.c
del bf\bf_opts.c
del bf\bfs.cpp
del bio\bf_lbuf.c
del bio\bss_rtcp.c
del bio\bss_dgram.c
rmdir /s /q bn\asm
rem del bn\bn_opt.c
rem del bn\bn_x931p.c
del bn\exp.c
rmdir /s /q camellia
del cast\c_enc.c
del cast\cast_spd.c
del cast\castopts.c
del cast\casts.cpp
rmdir /s /q cms
del conf\cnf_save.c
del conf\ssleay.cnf
del des\asm\des_enc.m4
rmdir /s /q des\t
rmdir /s /q des\times
del des\cbc3_enc.c
del des\des.c
del des\DES.pm
del des\des.pod
del des\DES.xs
del des\des-lib.com
del des\des_enc.c
rem del des\des_lib.c
del des\des_old.c
del des\des_old.h
del des\des_old2.c
del des\des_opts.c
del des\des3s.cpp
del des\dess.cpp
del des\fcrypt_b.c
del des\FILES0
del des\Imakefile
del des\KERBEROS
del des\options.txt
del des\read_pwd.c
del des\read2pwd.c
del des\rpw.c
del des\typemap
del dh\p1024.c
del dh\p192.c
del dh\p512.c
del dsa\fips186a.txt
del dsa\dsagen.c
del dso\dso_beos.c
rmdir /s /q engine
rem del evp\dig_eng.c
del evp\e_camellia.c
del evp\e_dsa.c
del evp\e_rc5.c
del evp\e_seed.c
rem del evp\evp_cnf.c
del evp\evptests.txt
del evp\m_md2.c
del evp\m_mdc2.c
del evp\openbsd_hw.c
del idea\idea_spd.c
rmdir /s /q jpake
rmdir /s /q md2
del md4\md4.c
del md4\md4s.cpp
del md5\md5.c
del md5\md5s.cpp
rmdir /s /q mdc2
del objects\objects.README
del pem\message
del pem\pkcs7.lis
rmdir perlasm
rmdir /s /q pkcs7\p7
rmdir /s /q pkcs7\t
del pkcs7\bio_ber.c
del pkcs7\dec.c
del pkcs7\doc
del pkcs7\enc.c
del pkcs7\example.c
del pkcs7\example.h
del pkcs7\pk7_dgst.c
del pkcs7\pk7_enc.c
del pkcs7\sign.c
del pkcs7\verify.c
rem del rand\rand_eng.c
del rand\rand_nw.c
del rand\rand_os2.c
del rand\rand_unix.c
del rand\randfile.c
del rc2\rrc2.doc
del rc2\tab.c
del rc4\rc4.c
rem del rc4\rc4_enc.c
rmdir /s /q rc4\asm
rem del rc4\rc4_fblk.c
del rc4\rc4s.cpp
del rc4\rrc4.doc
rmdir /s /q rc5
rmdir /s /q ripemd\asm
del ripemd\rmd160.c
rem del rsa\rsa_x931g.c
rmdir /s /q seed
rem del sha\asm\sha512-sse2.asm
del sha\sha.c
del sha\sha1.c
rem del sha\sha1s.cpp
del sha\sha256t.c
del sha\sha512t.c
rmdir /s /q store
rmdir /s /q threads
del x509v3\v3conf.c
del x509v3\v3prin.c
rem del cpu_win32.asm
del crypto-lib.com
rem del dyn_lck.c
rem del fips_err.c
rem del fips_err.h
del install-crypto.com
del LPdir_nyi.c
del LPdir_unix.c
del LPdir_wince.c
rem del o_init.c
del opensslconf.h.bak
del opensslconf.h.in

rem openssl-0.9.8k:
del aes\aes_x86core.c
rem del asn1\ameth_lib.c
rem del asn1\asn1_locl.h
del asn1\bio_asn1.c
del asn1\bio_ndef.c
del asn1\x_nx509.c

rmdir /s /q ec
rmdir /s /q ecdh
rmdir /s /q ecdsa
rmdir /s /q whrlpool

cd ..

cd ssl
del install-ssl.com
del Makefile
del ssl-lib.com
del ssl_task.c
del ssltest.c
cd ..

rmdir /s /q apps
rmdir /s /q bugs
rmdir /s /q certs
rmdir /s /q demos
rmdir /s /q doc
rmdir /s /q engines
rem rmdir /s /q fips
rmdir /s /q include
rmdir /s /q MacOS
rmdir /s /q ms
rmdir /s /q Netware
rmdir /s /q os2
rmdir /s /q perl
rmdir /s /q shlib
rmdir /s /q test
rmdir /s /q times
rmdir /s /q tools
rmdir /s /q util
rmdir /s /q VMS

del /q CHANGES.*
del config
del Configure
del FAQ
del /q INSTALL.*
del Makefile.bak
del Makefile.org
del Makefile.shared
del makevms.com
del MINFO
del NEWS
del openssl.doxy
del openssl.spec
del PROBLEMS
del /q README.*

ren crypto\des\asm\d-win32.asm d_win32.asm
ren crypto\des\asm\y-win32.asm y_win32.asm
ren crypto\bf\asm\b-win32.asm b_win32.asm
ren crypto\cast\asm\c-win32.asm c_win32.asm
rem ren crypto\rc4\asm\r4-win32.asm r4_win32.asm
ren crypto\md5\asm\m5-win32.asm m5_win32.asm
ren crypto\sha\asm\s1-win32.asm s1_win32.asm
rem ren crypto\ripemd\asm\rm-win32.asm rm_win32

set PHP=%~dp0\..\..\..\buildtools\php\php.exe
%PHP% %~dp0\fixasmalign.php crypto\des\asm\d_win32.asm 
%PHP% %~dp0\fixasmalign.php crypto\des\asm\y_win32.asm
%PHP% %~dp0\fixasmalign.php crypto\bf\asm\b_win32.asm
%PHP% %~dp0\fixasmalign.php crypto\cast\asm\c_win32.asm
%PHP% %~dp0\fixasmalign.php crypto\md5\asm\m5_win32.asm
%PHP% %~dp0\fixasmalign.php crypto\sha\asm\s1_win32.asm
