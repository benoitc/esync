#esync

Simple tool too synchronize folder in Erlang.

## Goal

When many hard disks around that contains identical folders and files copied from the laptops and computers here
during some emergencies (before travel, reinstallations, ...) this tools allows you to merge them.

esync copy one folder to another but keeps files in conflicts in the folder by appending to them the last modified time.

## Build

    $ make escript

## Usage

    $./escript SOURCE TARGET
