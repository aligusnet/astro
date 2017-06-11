stack build --allow-different-user
dd if=`stack path --local-install-root`/bin/astro-app > aws/main
cd aws
strip main
chmod +x main
