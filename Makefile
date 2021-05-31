build:
	docker run -v $(shell pwd):/build -v mocky-nix-store:/nix -w /build nixos/nix sh -c "nix-build docker.nix --argstr tag latest 1>&2 && cat result" | docker load

run:
	docker run -p 3000:3000/tcp mocky:latest

	
