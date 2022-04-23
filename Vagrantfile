# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.ssh.username = "root"
  config.ssh.insert_key = false
  config.ssh.extra_args = ["-o", "ControlMaster=auto",
                           "-o", "ControlPath=~/.ssh/vagrant-%C",
                           "-o", "ControlPersist=1h"]

  config.vm.define :lisp_test, primary: true do |node|
    node.vm.provider :docker do |d|
      d.name = "lisp-test"
      d.image = "ghcr.io/jiacai2050/devbox:v2"
      d.has_ssh = true
    end
  end

end
