# ResourcePackx24

Tool to allow easier exporting of 24x24 MineCraft resource packs in 1.17

## How to use

1. Run `ResourcePackx24.exe`
  - If this is the first time running this you will be prompted to enter the location of the working directory, the output directory, desired output size, and if these settings should be used without prompt in the future (this can be changed inside the `config.json` file, or just delete the file)
2. Upon startup files will be duplicated over and resized if they are 24x24
  - On future startups only file changes will be made
3. After initialisation has finished any changes in the working directory will be reflexted in the output directory.

## Notes
- `pack.lock` is there to track file changes while the application has been turned off. Don't edit, but you can delete it and restart to force a full refresh.
- This may not always work with every image editing program because they can use different methods to save files but this should suit most.
- The input and output directories should be the root folder for the resource pack. For instance `...\.minecraft\resourcepacks\my-resource-pack-24` and `...\.minecraft\resourcepacks\my-resource-pack-24-128`.
