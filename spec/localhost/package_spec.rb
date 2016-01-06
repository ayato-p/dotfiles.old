require 'spec_helper'

#-------------------------------------------------------------------------------
# sudo apt-get install -y ssh tmux zsh rxvt-unicode-256color stumpwm
#-------------------------------------------------------------------------------
describe package('ssh') do
  it { should be_installed }
end
describe package('tmux') do
  it { should be_installed }
end
describe package('zsh') do
  it { should be_installed }
end
describe package('rxvt-unicode-256color') do
  it { should be_installed }
end
describe package('stumpwm') do
  it { should be_installed }
end

#-------------------------------------------------------------------------------
# sudo apt-get install -y git silversearcher-ag
#-------------------------------------------------------------------------------
describe package('git') do
  it { should be_installed }
end
describe package('silversearcher-ag') do
  it { should be_installed }
end

#-------------------------------------------------------------------------------
# sudo apt-get install -y curl tree nkf ntpdate imagemagick xclip
#-------------------------------------------------------------------------------
describe package('curl') do
  it { should be_installed }
end
describe package('tree') do
  it { should be_installed }
end
describe package('nkf') do
  it { should be_installed }
end
describe package('ntpdate') do
  it { should be_installed }
end
describe package('imagemagick') do
  it { should be_installed }
end
describe package('xclip') do
  it { should be_installed }
end

#-------------------------------------------------------------------------------

# sudo apt-get install -y build-essential openssl keychain
#-------------------------------------------------------------------------------
describe package('build-essential') do
  it { should be_installed }
end
describe package('openssl') do
  it { should be_installed }
end
describe package('keychain') do
  it { should be_installed }
end

#-------------------------------------------------------------------------------
# sudo apt-get install -y autoconf automake libcurl4-gnutls-dev
#-------------------------------------------------------------------------------
describe package('autoconf') do
  it { should be_installed }
end
describe package('automake') do
  it { should be_installed }
end
describe package('libcurl4-gnutls-dev') do
  it { should be_installed }
end

#-------------------------------------------------------------------------------
# sudo apt-get install -y emacs
#-------------------------------------------------------------------------------
describe package('emacs') do
  it { should be_installed }
end

#-------------------------------------------------------------------------------
# sudo apt-get install -y fonts-inconsolata fonts-takao-gothic
#-------------------------------------------------------------------------------
describe package('fonts-inconsolata') do
  it { should be_installed }
end
describe package('fonts-takao-gothic') do
  it { should be_installed }
end

#-------------------------------------------------------------------------------
# sudo apt-get install -y openjdk-8-jdk # sbcl
#-------------------------------------------------------------------------------
describe package('openjdk-8-jdk') do
  it { should be_installed }
end

#-------------------------------------------------------------------------------
# sudo apt-get install -y google-chrome-stable
#-------------------------------------------------------------------------------
describe package('google-chrome-stable') do
  it { should be_installed }
end

#-------------------------------------------------------------------------------
# sudo apt-get install -y fcitx libskk-dev skkdic skkdic-extra fcitx-skk ddskk
#-------------------------------------------------------------------------------
describe package('fcitx') do
  it { should be_installed }
end
describe package('libskk-dev') do
  it { should be_installed }
end
describe package('skkdic') do
  it { should be_installed }
end
describe package('skkdic-extra') do
  it { should be_installed }
end
describe package('fcitx-skk') do
  it { should be_installed }
end
describe package('ddskk') do
  it { should be_installed }
end

#-------------------------------------------------------------------------------
# sudo apt-get install -y libssl-dev libreadline6-dev # for ruby build
#-------------------------------------------------------------------------------
describe package('libssl-dev') do
  it { should be_installed }
end
describe package('libreadline6-dev') do
  it { should be_installed }
end

#-------------------------------------------------------------------------------
# sudo apt-get install libxml2-dev libxslt1-dev
#-------------------------------------------------------------------------------
describe package('libxml2-dev') do
  it { should be_installed }
end
describe package('libxslt1-dev') do
  it { should be_installed }
end

#-------------------------------------------------------------------------------
# sudo apt-get install -y postgresql-9.4
#-------------------------------------------------------------------------------
describe package('postgresql-9.4') do
  it { should be_installed }
end
