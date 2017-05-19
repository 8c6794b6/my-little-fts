;;;; image.lisp - embedded image data

(in-package :my-little-fts)

(defparameter *alien-png*
  (make-array
   4434
   :element-type '(unsigned-byte 8)
   :initial-contents
   #(137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 64 0 0 0 38 8 6 0 0 0 116
     196 157 153 0 0 0 4 103 65 77 65 0 0 177 143 11 252 97 5 0 0 0 32 99 72 82 77
     0 0 122 38 0 0 128 132 0 0 250 0 0 0 128 232 0 0 117 48 0 0 234 96 0 0 58 152
     0 0 23 112 156 186 81 60 0 0 0 6 98 75 71 68 0 255 0 255 0 255 160 189 167
     147 0 0 0 9 112 72 89 115 0 0 11 19 0 0 11 19 1 0 154 156 24 0 0 0 7 116 73
     77 69 7 225 2 10 14 41 49 27 183 99 88 0 0 16 65 73 68 65 84 104 222 221 152
     105 148 93 85 149 199 255 231 220 241 189 123 223 80 85 239 213 60 87 165 138
     74 2 149 196 132 153 34 86 0 13 193 52 96 98 144 73 32 104 7 112 233 130 110
     151 138 139 68 180 87 103 209 14 171 109 141 3 32 130 51 1 68 77 43 104 48
     146 132 8 17 9 33 3 164 82 149 164 230 41 175 222 124 223 29 207 57 253 33
     160 210 13 209 132 216 105 123 175 117 215 253 240 214 221 103 255 127 103
     159 125 246 219 4 127 199 54 175 38 130 172 237 85 47 110 79 110 156 68 211
     207 151 220 254 220 119 36 50 26 216 156 251 7 71 28 123 249 210 114 97 115
     14 167 196 161 152 4 246 140 143 53 151 55 188 201 135 124 186 69 188 19 155
     93 101 98 50 95 58 171 171 58 209 221 114 214 29 115 202 180 233 149 57 23 51
     13 213 90 166 174 65 155 112 124 222 231 9 177 211 229 226 181 32 37 252 186
     110 29 27 126 50 134 59 174 174 251 255 1 224 7 15 191 31 31 249 240 19 231
     57 82 34 122 198 188 121 96 10 57 187 224 17 52 215 232 128 12 68 163 146 159
     115 216 196 145 113 231 241 130 207 190 60 250 146 63 54 87 10 225 43 223 27
     197 199 111 168 7 0 208 211 45 226 100 45 170 203 32 243 190 22 169 52 148
     243 120 180 3 21 201 164 200 230 124 174 42 132 233 26 229 49 67 230 121 155
     81 89 34 13 81 93 250 184 169 210 47 218 69 150 124 165 84 130 224 127 242
     243 119 155 1 23 52 197 49 85 176 155 12 61 124 102 144 156 3 74 85 20 138
     182 72 86 41 96 92 224 240 132 45 154 235 116 146 177 3 94 85 174 146 246 202
     208 202 189 125 214 240 158 215 172 181 201 168 236 126 233 129 17 112 71 251
     251 5 48 167 202 68 72 198 34 33 197 147 213 173 103 193 52 36 210 214 28 162
     90 136 16 63 16 136 26 50 31 207 120 188 165 74 167 84 0 133 2 163 245 149
     218 154 169 73 111 194 206 179 111 194 131 227 230 44 72 127 139 224 86 172
     88 129 138 138 10 172 88 177 2 221 221 221 168 174 174 198 185 231 158 139 87
     94 121 229 148 173 241 187 161 172 188 172 179 124 141 48 58 22 40 141 255 96
     25 17 221 227 66 100 50 22 59 152 182 252 1 195 144 212 218 132 22 11 184 16
     134 42 129 18 2 203 226 250 248 132 119 30 247 81 197 60 28 98 150 72 159 82
     0 203 151 47 71 44 22 195 217 103 159 141 245 235 215 99 219 182 109 145 100
     50 105 220 127 255 253 206 190 125 251 16 139 197 144 205 102 209 213 213 133
     241 241 241 19 246 191 248 222 75 80 185 168 6 141 113 3 233 254 84 178 183
     35 249 233 153 146 247 164 93 118 221 167 166 167 221 31 77 103 253 111 216
     62 127 200 225 226 135 37 206 159 205 121 65 133 237 243 102 238 67 125 237
     112 73 140 77 122 112 29 174 115 15 103 51 15 231 114 129 226 41 5 160 235 58
     118 237 218 37 95 126 249 229 11 251 251 251 175 168 174 174 254 124 127 127
     255 234 237 219 183 135 227 241 120 217 244 244 180 180 115 231 206 140 97 24
     34 157 78 159 144 239 119 223 123 9 102 210 25 60 241 165 175 226 201 177 93
     149 115 18 230 202 118 198 223 179 243 208 240 23 226 26 255 109 172 241 156
     97 45 68 83 106 136 20 181 48 181 117 131 14 59 132 111 38 192 120 58 23 232
     249 124 32 60 71 76 10 31 47 115 95 108 226 62 126 204 125 252 129 156 172
     216 139 238 121 55 8 57 246 185 25 9 129 185 28 171 151 175 134 189 203 122
     191 172 200 95 9 130 160 90 8 33 237 223 191 31 55 221 116 147 223 215 215
     231 110 220 184 113 48 22 139 173 33 148 236 216 159 232 195 246 207 255 246
     175 90 107 222 234 133 48 170 13 236 216 246 60 189 233 150 107 174 110 105
     111 184 179 152 154 152 231 254 98 239 244 214 221 227 151 70 163 250 0 93
     220 132 173 107 183 0 0 30 220 50 134 88 141 134 252 180 143 89 117 33 236
     121 205 42 167 1 202 124 75 4 190 45 178 165 241 32 7 137 192 47 9 252 213 0
     230 94 219 141 178 150 114 64 0 146 41 131 128 160 169 181 6 15 95 243 8 173
     190 161 73 159 59 167 51 89 204 20 26 238 186 248 206 79 171 174 122 121 192
     2 76 77 77 193 52 77 204 158 61 27 79 61 245 20 76 211 20 138 170 220 110 69
     138 223 220 116 240 87 8 27 26 142 246 167 176 251 59 47 189 237 186 23 124
     234 98 196 202 35 48 76 29 133 98 169 107 73 207 249 155 160 6 109 7 246 30
     64 162 165 220 59 58 93 248 242 230 71 255 176 174 101 118 141 183 237 222 45
     111 250 246 129 205 227 144 20 130 192 17 8 108 1 223 18 240 109 129 192 17
     32 84 224 147 119 52 253 229 91 96 241 231 46 65 116 97 5 210 59 167 161 170
     10 46 93 211 131 7 238 250 126 184 190 179 190 205 182 220 121 119 254 252 99
     11 162 134 217 93 22 141 213 69 77 179 140 123 165 184 106 233 104 137 182
     131 128 64 64 64 15 105 104 239 104 71 94 79 99 64 28 188 162 127 124 240 192
     161 131 163 59 58 102 55 250 201 89 137 183 92 183 247 95 47 131 66 37 228
     114 69 40 154 140 199 110 127 92 250 216 198 143 126 48 153 44 111 219 251
     234 43 144 94 30 133 31 81 85 51 174 95 16 45 55 12 74 136 247 223 125 124
     248 210 218 191 184 177 111 91 3 122 214 246 98 214 165 29 176 45 23 166 167
     96 243 191 61 45 53 157 223 210 89 156 42 174 106 93 216 118 119 235 220 182
     187 102 119 118 220 208 208 84 127 126 85 93 77 75 85 178 178 162 166 44 105
     68 203 77 9 49 134 76 46 141 134 250 6 4 97 23 41 50 137 104 99 24 229 109 81
     82 215 82 213 17 175 48 175 136 38 66 252 165 157 175 190 24 139 155 172 177
     167 5 67 91 143 252 73 252 231 46 197 150 141 207 97 86 119 51 158 93 247 27
     58 235 178 89 11 150 175 89 246 137 179 102 159 177 218 172 85 194 19 131 227
     8 146 33 112 67 193 196 72 230 135 91 239 253 205 47 27 47 122 179 143 191
     214 222 50 3 22 127 246 18 60 251 217 103 176 244 190 101 120 254 71 219 164
     229 31 95 254 174 235 190 117 227 117 145 154 216 178 186 170 202 214 72 36
     66 76 53 140 136 164 35 239 187 40 10 7 148 3 6 100 80 80 208 16 65 168 89
     194 164 53 4 61 169 35 172 68 64 4 1 5 133 9 19 101 205 229 137 144 20 186
     187 84 116 251 187 58 91 159 124 248 145 39 209 179 182 247 88 64 178 132 82
     201 193 215 127 114 47 54 254 232 233 150 143 61 126 199 135 231 180 119 220
     80 145 40 171 183 164 28 198 82 41 166 237 30 161 188 84 34 222 146 246 64
     150 165 162 96 37 113 241 186 43 78 88 60 128 255 89 3 46 94 183 4 249 92 17
     87 175 186 12 91 54 191 208 218 222 213 116 91 75 91 221 181 82 66 175 245 84
     32 44 105 8 81 21 30 103 208 169 2 133 72 160 144 80 161 24 208 169 138 0 62
     50 222 12 36 200 136 168 81 104 208 33 9 25 84 80 16 80 16 16 16 80 248 129
     135 23 246 238 218 190 233 233 103 174 169 74 38 198 103 50 25 8 16 68 99 97
     236 120 126 151 190 108 233 197 87 205 59 99 206 63 183 214 55 45 200 150 114
     185 131 163 3 191 57 154 77 239 72 15 204 144 185 47 28 249 244 20 196 239
     166 222 219 249 125 175 232 110 211 52 121 50 147 178 176 237 115 91 78 24
     192 155 142 64 207 218 94 8 159 99 231 23 119 144 154 69 181 151 207 89 216
     185 161 188 179 114 133 154 12 71 52 85 3 5 96 202 26 60 17 160 192 108 8 8
     40 148 162 196 61 56 220 135 74 37 248 196 198 145 129 49 204 28 40 33 239
     231 144 23 105 248 138 3 38 249 96 132 129 131 65 64 64 150 36 196 141 104
     131 174 233 53 67 19 163 35 169 220 76 169 224 20 148 120 36 214 125 105 239
     133 159 57 119 193 130 187 171 171 18 77 7 134 250 127 255 220 139 47 222 245
     195 239 254 242 11 203 222 53 103 251 47 62 242 227 195 166 38 181 14 28 201
     124 181 166 58 242 84 201 212 138 170 38 195 202 57 24 222 62 120 242 25 208
     179 182 23 204 103 216 177 126 43 185 234 129 149 215 119 207 239 188 175 182
     169 170 38 197 45 112 33 16 150 84 216 204 7 94 135 64 0 24 146 6 139 121 200
     4 37 40 68 66 157 30 131 44 121 56 178 123 10 109 211 103 34 86 22 195 140
     152 70 70 77 193 143 88 80 226 4 145 136 1 77 214 160 18 21 26 9 193 183 56
     166 142 30 157 76 101 51 195 16 132 39 226 101 77 137 202 178 154 130 151 23
     123 246 29 120 124 247 75 125 159 89 120 206 236 254 29 91 246 224 249 245
     219 222 136 217 0 80 2 192 79 88 241 219 101 64 221 249 141 104 155 223 136
     166 158 230 37 141 243 154 190 102 52 198 106 3 112 168 68 134 74 36 200 148
     32 23 148 64 8 7 19 1 70 188 20 198 188 25 112 184 8 224 33 34 203 208 36 1
     14 6 38 2 136 130 140 88 16 71 66 78 160 150 54 194 153 97 24 182 134 144 43
     230 145 43 20 81 176 11 176 88 1 92 243 96 196 117 51 145 140 215 37 42 99
     245 178 129 200 200 209 209 236 238 221 7 190 182 235 249 190 187 43 171 203
     71 191 125 219 119 209 122 94 219 159 23 57 15 128 120 167 226 129 63 43 130
     170 166 96 235 79 95 12 95 125 67 239 237 45 205 117 117 57 226 129 16 142 9
     63 131 108 96 33 44 201 112 185 143 144 36 67 2 193 164 159 133 66 36 168 84
     192 98 30 138 172 4 9 21 136 202 26 162 137 48 114 124 10 7 198 242 168 163
     141 8 10 28 83 116 12 213 109 101 80 52 10 43 231 160 127 223 216 14 219 114
     55 133 35 122 123 200 80 155 53 93 141 2 32 133 92 233 200 228 200 204 247
     182 110 218 253 235 5 23 117 122 138 42 227 156 15 93 112 82 231 251 132 0 36
     107 227 8 25 106 75 89 210 56 39 79 11 56 226 164 16 166 50 60 193 224 139 0
     4 20 89 86 66 42 224 168 81 35 72 42 38 162 146 14 135 251 72 251 37 16 1 68
     160 64 83 98 224 130 67 143 83 176 72 9 131 133 189 224 1 160 153 50 136 44
     33 224 28 106 72 129 16 226 185 135 175 253 254 125 132 16 180 92 55 87 175
     110 40 55 1 96 207 11 135 114 183 126 114 153 223 218 85 11 215 246 240 232
     173 143 253 77 132 191 97 4 120 253 206 63 179 30 133 76 233 131 11 223 221
     249 157 163 134 163 13 186 25 24 84 129 16 0 19 28 229 52 132 66 224 64 17 20
     49 170 99 202 47 128 8 32 38 105 8 56 71 152 42 72 202 6 84 42 3 132 128 80
     128 18 2 66 143 205 92 132 56 150 177 132 80 228 142 90 217 93 207 245 175 50
     99 161 95 15 246 77 190 101 96 127 171 29 127 219 12 120 112 197 35 184 245
     177 27 207 210 117 69 139 66 160 156 107 136 48 5 57 215 70 16 48 112 56 208
     57 7 133 0 151 60 132 5 1 8 129 144 24 64 4 4 4 132 16 224 130 31 235 0 57 1
     40 64 56 255 19 106 1 240 128 97 114 36 189 185 111 247 200 142 218 230 138
     255 85 177 199 5 208 116 205 28 227 170 213 23 205 38 148 192 202 23 193 74
     46 138 194 131 78 40 0 25 50 40 28 238 131 67 188 94 126 4 84 34 193 11 24 10
     194 67 129 184 208 116 9 229 90 24 146 42 1 132 192 177 92 94 42 184 105 198
     121 73 81 229 72 89 69 164 140 51 46 102 166 243 207 245 188 239 44 235 27
     239 123 232 180 9 127 19 0 51 22 66 99 123 101 76 15 41 29 132 0 196 18 224
     185 160 224 122 194 181 133 40 119 92 191 200 125 190 63 16 44 35 24 10 10 36
     203 22 190 67 64 108 9 212 119 69 192 9 37 4 178 175 166 21 173 161 190 61
     249 190 72 44 20 26 60 48 253 216 192 190 177 127 23 2 197 176 169 117 206
     191 112 214 55 227 9 35 161 40 146 78 41 65 207 218 222 211 186 251 127 4 80
     150 48 161 168 114 163 30 86 171 38 70 210 163 135 247 78 126 33 51 83 216
     163 42 114 119 87 119 211 125 57 203 218 246 236 175 246 124 200 57 84 42 216
     131 150 56 122 100 90 200 128 8 224 190 113 21 9 0 68 8 33 206 191 123 113 99
     60 105 158 167 168 82 245 204 84 254 231 101 149 145 23 102 38 243 24 61 124
     116 226 140 249 13 35 241 138 112 66 209 228 88 215 252 70 236 221 121 226
     189 251 41 7 208 115 79 47 84 93 129 234 6 179 36 133 198 166 198 50 63 123
     244 214 199 54 92 189 225 42 38 203 82 107 216 208 180 236 76 241 240 193 7
     119 101 46 190 119 137 128 0 4 159 245 86 190 196 138 111 93 141 120 185 1 66
     32 149 10 78 193 115 131 131 102 76 71 224 51 204 76 21 28 207 13 134 25 231
     243 21 85 142 125 180 118 29 237 89 219 251 142 27 153 119 12 0 4 88 212 219
     129 223 63 115 96 22 231 156 230 102 172 151 110 219 116 51 139 71 35 24 31
     78 53 0 128 85 116 134 111 123 242 22 177 255 229 193 227 166 236 53 15 173
     132 172 6 38 149 136 98 151 188 84 122 186 48 30 248 12 0 48 252 232 254 146
     253 193 133 67 140 113 200 10 141 151 189 167 69 1 224 158 110 0 20 2 184 231
     198 71 180 112 84 111 243 28 191 104 91 238 190 244 116 1 235 175 219 32 233
     97 181 150 49 238 187 142 63 84 178 92 8 126 252 230 75 86 37 200 178 20 39
     148 168 158 235 143 76 14 167 139 133 108 9 0 112 199 47 111 17 86 222 25 230
     156 131 202 52 26 175 48 149 211 45 30 0 168 164 80 212 181 84 24 170 38 183
     187 142 63 157 157 41 30 202 166 138 104 94 220 165 171 170 92 29 248 204 98
     1 27 242 189 0 219 255 229 248 35 44 73 162 160 18 169 32 4 154 231 248 195
     135 95 157 180 223 248 205 42 184 112 74 222 48 243 153 71 41 137 170 186 242
     127 98 36 79 227 9 19 181 205 21 229 146 76 27 93 199 239 31 61 52 147 117
     109 31 209 88 88 151 21 169 198 247 89 46 159 43 77 20 243 246 113 29 245 172
     237 133 36 19 80 137 36 8 129 102 91 238 216 125 27 111 246 129 99 247 188
     239 6 96 1 27 113 29 191 68 40 49 85 77 150 169 116 210 35 201 83 7 192 136
     104 8 153 106 139 164 208 168 93 116 251 6 190 187 187 104 70 195 168 172 137
     135 41 165 85 204 103 35 147 99 25 43 151 181 142 239 137 0 138 42 67 213 229
     164 0 80 42 122 227 175 254 97 228 143 29 96 169 232 160 84 244 38 93 199 47
     18 130 176 17 209 36 77 63 253 167 128 18 74 160 104 114 155 44 83 165 152
     179 251 110 254 241 181 34 172 171 48 163 161 4 149 104 196 243 252 225 169
     177 116 233 120 231 127 201 146 37 0 3 238 90 126 37 244 144 154 96 1 243 3
     159 77 6 62 195 208 195 135 80 85 85 133 220 140 133 163 99 217 98 224 177
     105 16 104 90 72 165 138 122 250 79 1 253 222 250 103 180 80 88 157 227 123
     204 183 114 238 225 135 86 253 0 239 189 109 9 0 212 72 18 213 237 146 55 248
     137 117 31 112 201 110 129 165 75 151 226 202 43 175 196 170 85 171 112 227
     141 55 226 250 235 175 199 202 149 43 97 24 6 228 163 18 58 231 255 163 172
     104 114 34 240 185 195 25 159 82 203 20 128 0 132 18 48 0 169 201 130 29 4
     108 146 7 28 66 8 206 24 71 207 186 222 211 11 224 186 79 46 249 82 85 67 217
     7 20 77 86 194 17 237 130 218 57 77 209 31 220 253 196 162 104 220 184 77 11
     41 122 224 177 182 167 30 216 89 79 83 8 3 8 1 208 95 127 155 0 98 0 202 0
     148 169 3 82 124 94 109 115 13 5 169 11 124 230 101 135 10 246 207 150 62 165
     3 208 5 19 234 214 123 158 145 106 26 226 21 129 207 53 198 132 172 104 114
     195 35 255 116 39 108 203 59 249 232 79 129 73 23 174 94 116 127 36 30 170
     166 148 74 130 243 216 232 64 234 137 142 243 155 63 212 214 85 115 139 162
     200 36 100 168 237 217 153 226 1 183 207 27 85 13 53 42 73 82 84 150 229 152
     44 203 49 33 132 201 24 11 113 198 53 155 186 225 250 165 201 91 106 218 203
     223 175 104 178 233 123 129 99 59 254 203 165 113 139 57 5 7 179 62 208 21 91
     112 89 199 134 154 230 242 247 154 49 61 174 135 149 119 125 251 167 155 183
     84 84 69 210 7 126 209 119 218 0 200 227 131 233 53 19 131 153 58 73 166 17
     223 246 135 114 175 164 179 185 243 10 207 14 107 211 13 174 237 151 20 69
     162 206 180 247 154 164 83 9 199 254 211 209 63 123 142 21 6 1 17 168 44 84
     86 107 46 86 53 57 38 132 64 69 125 236 42 163 209 120 112 250 249 201 60 243
     24 244 136 170 82 137 196 178 41 107 204 119 131 34 103 188 64 0 38 78 201 92
     231 29 0 208 66 202 127 118 175 56 19 159 137 125 150 0 144 171 43 171 229
     221 27 246 108 87 107 195 47 42 190 68 116 67 87 162 196 16 122 88 87 223 214
     11 1 209 108 165 56 177 63 253 31 169 209 252 211 66 112 198 153 152 204 191
     150 59 170 68 21 130 176 32 3 63 233 79 69 170 205 155 149 176 98 122 110 144
     39 148 120 91 54 190 52 125 230 133 237 167 23 192 192 211 67 120 252 214 39
     161 81 13 122 88 11 136 76 2 234 2 217 93 19 118 85 188 22 106 57 37 164 150
     40 0 52 0 97 66 136 142 99 187 31 0 240 1 184 32 240 36 80 111 234 235 169 77
     191 159 124 33 192 235 153 209 214 212 42 81 153 18 48 194 185 205 120 205
     220 170 97 107 218 130 162 202 0 1 22 189 103 54 66 97 245 164 131 63 21 246
     95 200 68 125 228 82 163 55 148 0 0 0 37 116 69 88 116 100 97 116 101 58 99
     114 101 97 116 101 0 50 48 49 55 45 48 50 45 49 48 84 49 52 58 52 49 58 53 49
     43 48 57 58 48 48 125 128 117 59 0 0 0 37 116 69 88 116 100 97 116 101 58 109
     111 100 105 102 121 0 50 48 49 55 45 48 50 45 49 48 84 49 52 58 52 49 58 52
     57 43 48 57 58 48 48 243 152 131 126 0 0 0 0 73 69 78 68 174 66 96 130))
  "Lisp alien PNG image data as octet vector.")
