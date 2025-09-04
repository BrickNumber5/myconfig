#! /usr/bin/python3

# Utility for taking the extracted search.json from firefox getting images from
# "data:<mimetype>;base64,<data>" urls in the file and extracting them into
# a sidecar directory containing the images.
# Can also perform the reverse operation.
# 
# Dependencies: python3
# (sys, os, json, base64, mimetypes)
# 
# The basic reason I want this is because I'd prefer not to check in a
# search.json file containing a bunch of base64-encoded images to the git repo.

# This utility expects the mode of operation 'extract' or 'combine', the file
# to perform the operation on, and the image directory.

# We automatically detect the relevant name by searching first for a
# `._metaData.alias` then an `.id` and append `-` followed by the innermost key.
# The extension is chosen mostly via mimetypes.guess_extension, meaning, e.g.,
# ```
# {
#   ...,
#   _metaData: { ..., alias: "@mdn" },
#   _iconMapObj: { ..., "48": "data:image/png;base64,..." }
# }
# ```
# will become
# ```
# {
#   ...,
#   _metaData: { ..., alias: "@mdn" },
#   _iconMapObj: { ..., "48": "sidecar:image/png,mdn-48.png" }
# }
# ```

import sys
import os
import json
import base64
import mimetypes

# mimetype's source (probably /etc/mime.types) doesn't have a mapping from
# image/x-icon to .ico, add one
mimetypes.add_type('image/x-icon', '.ico')

mode     = sys.argv[1]
filename = sys.argv[2]
sidecar  = sys.argv[3]

def extract(obj, name = '', size = ''):
    if type(obj) is str and obj.startswith('data:'):
        obj = obj.removeprefix('data:').replace(';base64', '', count = 1)
        mime, _, data = obj.partition(',')
        
        ext = mimetypes.guess_extension(mime) or ''
        filename = f"{name}-{size}{ext}"
        
        data = base64.b64decode(data)
        with open(os.path.join(sidecar, filename), 'wb') as file:
            file.write(data)
        
        return f"sidecar:{mime},{filename}"
    elif type(obj) is dict:
        name = obj.get('_metaData', {}).get('alias') or obj.get('id') or name
        name = name.replace('@', '')
        
        return { k: extract(v, name, k) for k, v in obj.items() }
    elif type(obj) is list:
        return [ extract(v, name, size) for v in obj ]
    
    return obj

def combine(obj):
    if type(obj) is str and obj.startswith('sidecar:'):
        obj = obj.removeprefix('sidecar:')
        mime, _, filename = obj.partition(',')
        
        with open(os.path.join(sidecar, filename), 'rb') as file:
            data = base64.b64encode(file.read()).decode('UTF-8')
        
        return f"data:{mime};base64,{data}"
    elif type(obj) is dict:
        return { k: combine(v) for k, v in obj.items() }
    elif type(obj) is list:
        return [ combine(v) for v in obj ]
    
    return obj

with open(filename, 'r', encoding = 'UTF-8') as file:
    data = json.load(file)

    if mode == 'extract':
        os.makedirs(sidecar, exist_ok = True)
        data = extract(data)
    elif mode == 'combine':
        data = combine(data)

with open(filename, 'w', encoding = 'UTF-8', newline = '\n') as file:
    json.dump(data, file, indent = 4)
    file.write('\n')
