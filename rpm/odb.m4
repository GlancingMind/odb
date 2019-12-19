Summary: Object Persistency Framework (ODB)
Name: odb
Version: VERSION
Release: BUILD
Copyright: GPL
Group: Development/Tools
Source: odb.tar.gz
BuildRoot: /tmp/%{name}-buildroot
AutoReqProv: no

%description
This package contains tools and libraries for the implementation
of object persistency for Ada 95.
%prep
%setup -n odb-src-VERSION
%build
./configure 
make 
%install
make install ROOT=$RPM_BUILD_ROOT/usr/local/odb

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc README COPYING TODO AUTHORS
/usr/local/odb/include/*.ads
/usr/local/odb/include/*.adb
/usr/local/odb/include/*.ali
/usr/local/odb/lib/libodb*.a
/usr/local/odb/lib/libodb*.so
/usr/local/odb/bin/odl
/usr/local/odb/bin/odlls
/usr/local/odb/bin/odb-config

%changelog
* Mon Oct 13 2003 Michael Erdmann <michael.erdmann@snafu.de>
- first attempt

