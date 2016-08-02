-- -*- haskell -*-
--
-- Lexical syntax for illiterate Haskell 98.
--
-- (c) Simon Marlow 2003, with the caveat that much of this is
-- translated directly from the syntax in the Haskell 98 report.
--

{
{-# OPTIONS -w  #-}
module Yi.Lexer.Haskell ( initState, alexScanToken, tokenToStyle,
                          tokenToText,
                          TT, isErrorTok, isSpecial,
                          startsLayout, isComment, Token(..), HlState, CommentType(..), ReservedType(..), OpType(..) ) where
import Yi.Lexer.Alex hiding (tokenToStyle)
import Yi.Style
}

$whitechar = [\ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}]

$ascdigit  = 0-9
$unidigit  = [] -- GHC 8.0.1 Doesn't support unicode decimal digits
$digit     = [$ascdigit $unidigit]

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [\x9\xb\x1f\x24\x2b\x3c-\x3e\x5e\x60\x7c\x7e\xa2-\xa6\xa8\xa9\xac\xae-\xb1\xb4\xb8\xd7\xf7\x2c2-\x2c5\x2d2-\x2df\x2e5-\x2eb\x2ed\x2ef-\x2ff\x375\x384\x385\x3f6\x482\x58d-\x58f\x606-\x608\x60b\x60e\x60f\x6de\x6e9\x6fd\x6fe\x7f6\x9f2\x9f3\x9fa\x9fb\xaf1\xb70\xbf3-\xbfa\xc7f\xd4f\xd79\xe3f\xf01-\xf03\xf13\xf15-\xf17\xf1a-\xf1f\xf34\xf36\xf38\xfbe-\xfc5\xfc7-\xfcc\xfce\xfcf\xfd5-\xfd8\x109e\x109f\x1390-\x1399\x17db\x1940\x19de-\x19ff\x1b61-\x1b6a\x1b74-\x1b7c\x1fbd\x1fbf-\x1fc1\x1fcd-\x1fcf\x1fdd-\x1fdf\x1fed-\x1fef\x1ffd\x1ffe\x2044\x2052\x207a-\x207c\x208a-\x208c\x20a0-\x20be\x2100\x2101\x2103-\x2106\x2108\x2109\x2114\x2116-\x2118\x211e-\x2123\x2125\x2127\x2129\x212e\x213a\x213b\x2140-\x2144\x214a-\x214d\x214f\x218a\x218b\x2190-\x2307\x230c-\x2328\x232b-\x23fe\x2400-\x2426\x2440-\x244a\x249c-\x24e9\x2500-\x2767\x2794-\x27c4\x27c7-\x27e5\x27f0-\x2982\x2999-\x29d7\x29dc-\x29fb\x29fe-\x2b73\x2b76-\x2b95\x2b98-\x2bb9\x2bbd-\x2bc8\x2bca-\x2bd1\x2bec-\x2bef\x2ce5-\x2cea\x2e80-\x2e99\x2e9b-\x2ef3\x2f00-\x2fd5\x2ff0-\x2ffb\x3004\x3012\x3013\x3020\x3036\x3037\x303e\x303f\x309b\x309c\x3190\x3191\x3196-\x319f\x31c0-\x31e3\x3200-\x321e\x322a-\x3247\x3250\x3260-\x327f\x328a-\x32b0\x32c0-\x32fe\x3300-\x33ff\x4dc0-\x4dff\xa490-\xa4c6\xa700-\xa716\xa720\xa721\xa789\xa78a\xa828-\xa82b\xa836-\xa839\xaa77-\xaa79\xab5b\xfb29\xfbb2-\xfbc1\xfdfc\xfdfd\xfe62\xfe64-\xfe66\xfe69\xff04\xff0b\xff1c-\xff1e\xff3e\xff40\xff5c\xff5e\xffe0-\xffe6\xffe8-\xffee\xfffc\xfffd\x10137-\x1013f\x10179-\x10189\x1018c-\x1018e\x10190-\x1019b\x101a0\x101d0-\x101fc\x10877\x10878\x10ac8\x1173f\x16b3c-\x16b3f\x16b45\x1bc9c\x1d000-\x1d0f5\x1d100-\x1d126\x1d129-\x1d164\x1d16a-\x1d16c\x1d183\x1d184\x1d18c-\x1d1a9\x1d1ae-\x1d1e8\x1d200-\x1d241\x1d245\x1d300-\x1d356\x1d6c1\x1d6db\x1d6fb\x1d715\x1d735\x1d74f\x1d76f\x1d789\x1d7a9\x1d7c3\x1d800-\x1d9ff\x1da37-\x1da3a\x1da6d-\x1da74\x1da76-\x1da83\x1da85\x1da86\x1eef0\x1eef1\x1f000-\x1f02b\x1f030-\x1f093\x1f0a0-\x1f0ae\x1f0b1-\x1f0bf\x1f0c1-\x1f0cf\x1f0d1-\x1f0f5\x1f110-\x1f12e\x1f130-\x1f16b\x1f170-\x1f1ac\x1f1e6-\x1f202\x1f210-\x1f23b\x1f240-\x1f248\x1f250\x1f251\x1f300-\x1f6d2\x1f6e0-\x1f6ec\x1f6f0-\x1f6f6\x1f700-\x1f773\x1f780-\x1f7d4\x1f800-\x1f80b\x1f810-\x1f847\x1f850-\x1f859\x1f860-\x1f887\x1f890-\x1f8ad\x1f910-\x1f91e\x1f920-\x1f927\x1f930\x1f933-\x1f93e\x1f940-\x1f94b\x1f950-\x1f95e\x1f980-\x1f991\x1f9c0]
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']

$large = [\x41-\x5a\xc0-\xd6\xd8-\xde\x100\x102\x104\x106\x108\x10a\x10c\x10e\x110\x112\x114\x116\x118\x11a\x11c\x11e\x120\x122\x124\x126\x128\x12a\x12c\x12e\x130\x132\x134\x136\x139\x13b\x13d\x13f\x141\x143\x145\x147\x14a\x14c\x14e\x150\x152\x154\x156\x158\x15a\x15c\x15e\x160\x162\x164\x166\x168\x16a\x16c\x16e\x170\x172\x174\x176\x178\x179\x17b\x17d\x181\x182\x184\x186\x187\x189-\x18b\x18e-\x191\x193\x194\x196-\x198\x19c\x19d\x19f\x1a0\x1a2\x1a4\x1a6\x1a7\x1a9\x1ac\x1ae\x1af\x1b1-\x1b3\x1b5\x1b7\x1b8\x1bc\x1c4\x1c7\x1ca\x1cd\x1cf\x1d1\x1d3\x1d5\x1d7\x1d9\x1db\x1de\x1e0\x1e2\x1e4\x1e6\x1e8\x1ea\x1ec\x1ee\x1f1\x1f4\x1f6-\x1f8\x1fa\x1fc\x1fe\x200\x202\x204\x206\x208\x20a\x20c\x20e\x210\x212\x214\x216\x218\x21a\x21c\x21e\x220\x222\x224\x226\x228\x22a\x22c\x22e\x230\x232\x23a\x23b\x23d\x23e\x241\x243-\x246\x248\x24a\x24c\x24e\x370\x372\x376\x37f\x386\x388-\x38a\x38c\x38e\x38f\x391-\x3a1\x3a3-\x3ab\x3cf\x3d2-\x3d4\x3d8\x3da\x3dc\x3de\x3e0\x3e2\x3e4\x3e6\x3e8\x3ea\x3ec\x3ee\x3f4\x3f7\x3f9\x3fa\x3fd-\x42f\x460\x462\x464\x466\x468\x46a\x46c\x46e\x470\x472\x474\x476\x478\x47a\x47c\x47e\x480\x48a\x48c\x48e\x490\x492\x494\x496\x498\x49a\x49c\x49e\x4a0\x4a2\x4a4\x4a6\x4a8\x4aa\x4ac\x4ae\x4b0\x4b2\x4b4\x4b6\x4b8\x4ba\x4bc\x4be\x4c0\x4c1\x4c3\x4c5\x4c7\x4c9\x4cb\x4cd\x4d0\x4d2\x4d4\x4d6\x4d8\x4da\x4dc\x4de\x4e0\x4e2\x4e4\x4e6\x4e8\x4ea\x4ec\x4ee\x4f0\x4f2\x4f4\x4f6\x4f8\x4fa\x4fc\x4fe\x500\x502\x504\x506\x508\x50a\x50c\x50e\x510\x512\x514\x516\x518\x51a\x51c\x51e\x520\x522\x524\x526\x528\x52a\x52c\x52e\x531-\x556\x10a0-\x10c5\x10c7\x10cd\x13a0-\x13f5\x1e00\x1e02\x1e04\x1e06\x1e08\x1e0a\x1e0c\x1e0e\x1e10\x1e12\x1e14\x1e16\x1e18\x1e1a\x1e1c\x1e1e\x1e20\x1e22\x1e24\x1e26\x1e28\x1e2a\x1e2c\x1e2e\x1e30\x1e32\x1e34\x1e36\x1e38\x1e3a\x1e3c\x1e3e\x1e40\x1e42\x1e44\x1e46\x1e48\x1e4a\x1e4c\x1e4e\x1e50\x1e52\x1e54\x1e56\x1e58\x1e5a\x1e5c\x1e5e\x1e60\x1e62\x1e64\x1e66\x1e68\x1e6a\x1e6c\x1e6e\x1e70\x1e72\x1e74\x1e76\x1e78\x1e7a\x1e7c\x1e7e\x1e80\x1e82\x1e84\x1e86\x1e88\x1e8a\x1e8c\x1e8e\x1e90\x1e92\x1e94\x1e9e\x1ea0\x1ea2\x1ea4\x1ea6\x1ea8\x1eaa\x1eac\x1eae\x1eb0\x1eb2\x1eb4\x1eb6\x1eb8\x1eba\x1ebc\x1ebe\x1ec0\x1ec2\x1ec4\x1ec6\x1ec8\x1eca\x1ecc\x1ece\x1ed0\x1ed2\x1ed4\x1ed6\x1ed8\x1eda\x1edc\x1ede\x1ee0\x1ee2\x1ee4\x1ee6\x1ee8\x1eea\x1eec\x1eee\x1ef0\x1ef2\x1ef4\x1ef6\x1ef8\x1efa\x1efc\x1efe\x1f08-\x1f0f\x1f18-\x1f1d\x1f28-\x1f2f\x1f38-\x1f3f\x1f48-\x1f4d\x1f59\x1f5b\x1f5d\x1f5f\x1f68-\x1f6f\x1fb8-\x1fbb\x1fc8-\x1fcb\x1fd8-\x1fdb\x1fe8-\x1fec\x1ff8-\x1ffb\x2102\x2107\x210b-\x210d\x2110-\x2112\x2115\x2119-\x211d\x2124\x2126\x2128\x212a-\x212d\x2130-\x2133\x213e\x213f\x2145\x2183\x2c00-\x2c2e\x2c60\x2c62-\x2c64\x2c67\x2c69\x2c6b\x2c6d-\x2c70\x2c72\x2c75\x2c7e-\x2c80\x2c82\x2c84\x2c86\x2c88\x2c8a\x2c8c\x2c8e\x2c90\x2c92\x2c94\x2c96\x2c98\x2c9a\x2c9c\x2c9e\x2ca0\x2ca2\x2ca4\x2ca6\x2ca8\x2caa\x2cac\x2cae\x2cb0\x2cb2\x2cb4\x2cb6\x2cb8\x2cba\x2cbc\x2cbe\x2cc0\x2cc2\x2cc4\x2cc6\x2cc8\x2cca\x2ccc\x2cce\x2cd0\x2cd2\x2cd4\x2cd6\x2cd8\x2cda\x2cdc\x2cde\x2ce0\x2ce2\x2ceb\x2ced\x2cf2\xa640\xa642\xa644\xa646\xa648\xa64a\xa64c\xa64e\xa650\xa652\xa654\xa656\xa658\xa65a\xa65c\xa65e\xa660\xa662\xa664\xa666\xa668\xa66a\xa66c\xa680\xa682\xa684\xa686\xa688\xa68a\xa68c\xa68e\xa690\xa692\xa694\xa696\xa698\xa69a\xa722\xa724\xa726\xa728\xa72a\xa72c\xa72e\xa732\xa734\xa736\xa738\xa73a\xa73c\xa73e\xa740\xa742\xa744\xa746\xa748\xa74a\xa74c\xa74e\xa750\xa752\xa754\xa756\xa758\xa75a\xa75c\xa75e\xa760\xa762\xa764\xa766\xa768\xa76a\xa76c\xa76e\xa779\xa77b\xa77d\xa77e\xa780\xa782\xa784\xa786\xa78b\xa78d\xa790\xa792\xa796\xa798\xa79a\xa79c\xa79e\xa7a0\xa7a2\xa7a4\xa7a6\xa7a8\xa7aa-\xa7ae\xa7b0-\xa7b4\xa7b6\xff21-\xff3a\x10400-\x10427\x104b0-\x104d3\x10c80-\x10cb2\x118a0-\x118bf\x1d400-\x1d419\x1d434-\x1d44d\x1d468-\x1d481\x1d49c\x1d49e\x1d49f\x1d4a2\x1d4a5\x1d4a6\x1d4a9-\x1d4ac\x1d4ae-\x1d4b5\x1d4d0-\x1d4e9\x1d504\x1d505\x1d507-\x1d50a\x1d50d-\x1d514\x1d516-\x1d51c\x1d538\x1d539\x1d53b-\x1d53e\x1d540-\x1d544\x1d546\x1d54a-\x1d550\x1d56c-\x1d585\x1d5a0-\x1d5b9\x1d5d4-\x1d5ed\x1d608-\x1d621\x1d63c-\x1d655\x1d670-\x1d689\x1d6a8-\x1d6c0\x1d6e2-\x1d6fa\x1d71c-\x1d734\x1d756-\x1d76e\x1d790-\x1d7a8\x1d7ca\x1e900-\x1e921\x1c5\x1c8\x1cb\x1f2\x1f88-\x1f8f\x1f98-\x1f9f\x1fa8-\x1faf\x1fbc\x1fcc\x1ffc]
$small = [\x61-\x7a\xb5\xdf-\xf6\xf8-\xff\x101\x103\x105\x107\x109\x10b\x10d\x10f\x111\x113\x115\x117\x119\x11b\x11d\x11f\x121\x123\x125\x127\x129\x12b\x12d\x12f\x131\x133\x135\x137\x138\x13a\x13c\x13e\x140\x142\x144\x146\x148\x149\x14b\x14d\x14f\x151\x153\x155\x157\x159\x15b\x15d\x15f\x161\x163\x165\x167\x169\x16b\x16d\x16f\x171\x173\x175\x177\x17a\x17c\x17e-\x180\x183\x185\x188\x18c\x18d\x192\x195\x199-\x19b\x19e\x1a1\x1a3\x1a5\x1a8\x1aa\x1ab\x1ad\x1b0\x1b4\x1b6\x1b9\x1ba\x1bd-\x1bf\x1c6\x1c9\x1cc\x1ce\x1d0\x1d2\x1d4\x1d6\x1d8\x1da\x1dc\x1dd\x1df\x1e1\x1e3\x1e5\x1e7\x1e9\x1eb\x1ed\x1ef\x1f0\x1f3\x1f5\x1f9\x1fb\x1fd\x1ff\x201\x203\x205\x207\x209\x20b\x20d\x20f\x211\x213\x215\x217\x219\x21b\x21d\x21f\x221\x223\x225\x227\x229\x22b\x22d\x22f\x231\x233-\x239\x23c\x23f\x240\x242\x247\x249\x24b\x24d\x24f-\x293\x295-\x2af\x371\x373\x377\x37b-\x37d\x390\x3ac-\x3ce\x3d0\x3d1\x3d5-\x3d7\x3d9\x3db\x3dd\x3df\x3e1\x3e3\x3e5\x3e7\x3e9\x3eb\x3ed\x3ef-\x3f3\x3f5\x3f8\x3fb\x3fc\x430-\x45f\x461\x463\x465\x467\x469\x46b\x46d\x46f\x471\x473\x475\x477\x479\x47b\x47d\x47f\x481\x48b\x48d\x48f\x491\x493\x495\x497\x499\x49b\x49d\x49f\x4a1\x4a3\x4a5\x4a7\x4a9\x4ab\x4ad\x4af\x4b1\x4b3\x4b5\x4b7\x4b9\x4bb\x4bd\x4bf\x4c2\x4c4\x4c6\x4c8\x4ca\x4cc\x4ce\x4cf\x4d1\x4d3\x4d5\x4d7\x4d9\x4db\x4dd\x4df\x4e1\x4e3\x4e5\x4e7\x4e9\x4eb\x4ed\x4ef\x4f1\x4f3\x4f5\x4f7\x4f9\x4fb\x4fd\x4ff\x501\x503\x505\x507\x509\x50b\x50d\x50f\x511\x513\x515\x517\x519\x51b\x51d\x51f\x521\x523\x525\x527\x529\x52b\x52d\x52f\x561-\x587\x13f8-\x13fd\x1c80-\x1c88\x1d00-\x1d2b\x1d6b-\x1d77\x1d79-\x1d9a\x1e01\x1e03\x1e05\x1e07\x1e09\x1e0b\x1e0d\x1e0f\x1e11\x1e13\x1e15\x1e17\x1e19\x1e1b\x1e1d\x1e1f\x1e21\x1e23\x1e25\x1e27\x1e29\x1e2b\x1e2d\x1e2f\x1e31\x1e33\x1e35\x1e37\x1e39\x1e3b\x1e3d\x1e3f\x1e41\x1e43\x1e45\x1e47\x1e49\x1e4b\x1e4d\x1e4f\x1e51\x1e53\x1e55\x1e57\x1e59\x1e5b\x1e5d\x1e5f\x1e61\x1e63\x1e65\x1e67\x1e69\x1e6b\x1e6d\x1e6f\x1e71\x1e73\x1e75\x1e77\x1e79\x1e7b\x1e7d\x1e7f\x1e81\x1e83\x1e85\x1e87\x1e89\x1e8b\x1e8d\x1e8f\x1e91\x1e93\x1e95-\x1e9d\x1e9f\x1ea1\x1ea3\x1ea5\x1ea7\x1ea9\x1eab\x1ead\x1eaf\x1eb1\x1eb3\x1eb5\x1eb7\x1eb9\x1ebb\x1ebd\x1ebf\x1ec1\x1ec3\x1ec5\x1ec7\x1ec9\x1ecb\x1ecd\x1ecf\x1ed1\x1ed3\x1ed5\x1ed7\x1ed9\x1edb\x1edd\x1edf\x1ee1\x1ee3\x1ee5\x1ee7\x1ee9\x1eeb\x1eed\x1eef\x1ef1\x1ef3\x1ef5\x1ef7\x1ef9\x1efb\x1efd\x1eff-\x1f07\x1f10-\x1f15\x1f20-\x1f27\x1f30-\x1f37\x1f40-\x1f45\x1f50-\x1f57\x1f60-\x1f67\x1f70-\x1f7d\x1f80-\x1f87\x1f90-\x1f97\x1fa0-\x1fa7\x1fb0-\x1fb4\x1fb6\x1fb7\x1fbe\x1fc2-\x1fc4\x1fc6\x1fc7\x1fd0-\x1fd3\x1fd6\x1fd7\x1fe0-\x1fe7\x1ff2-\x1ff4\x1ff6\x1ff7\x210a\x210e\x210f\x2113\x212f\x2134\x2139\x213c\x213d\x2146-\x2149\x214e\x2184\x2c30-\x2c5e\x2c61\x2c65\x2c66\x2c68\x2c6a\x2c6c\x2c71\x2c73\x2c74\x2c76-\x2c7b\x2c81\x2c83\x2c85\x2c87\x2c89\x2c8b\x2c8d\x2c8f\x2c91\x2c93\x2c95\x2c97\x2c99\x2c9b\x2c9d\x2c9f\x2ca1\x2ca3\x2ca5\x2ca7\x2ca9\x2cab\x2cad\x2caf\x2cb1\x2cb3\x2cb5\x2cb7\x2cb9\x2cbb\x2cbd\x2cbf\x2cc1\x2cc3\x2cc5\x2cc7\x2cc9\x2ccb\x2ccd\x2ccf\x2cd1\x2cd3\x2cd5\x2cd7\x2cd9\x2cdb\x2cdd\x2cdf\x2ce1\x2ce3\x2ce4\x2cec\x2cee\x2cf3\x2d00-\x2d25\x2d27\x2d2d\xa641\xa643\xa645\xa647\xa649\xa64b\xa64d\xa64f\xa651\xa653\xa655\xa657\xa659\xa65b\xa65d\xa65f\xa661\xa663\xa665\xa667\xa669\xa66b\xa66d\xa681\xa683\xa685\xa687\xa689\xa68b\xa68d\xa68f\xa691\xa693\xa695\xa697\xa699\xa69b\xa723\xa725\xa727\xa729\xa72b\xa72d\xa72f-\xa731\xa733\xa735\xa737\xa739\xa73b\xa73d\xa73f\xa741\xa743\xa745\xa747\xa749\xa74b\xa74d\xa74f\xa751\xa753\xa755\xa757\xa759\xa75b\xa75d\xa75f\xa761\xa763\xa765\xa767\xa769\xa76b\xa76d\xa76f\xa771-\xa778\xa77a\xa77c\xa77f\xa781\xa783\xa785\xa787\xa78c\xa78e\xa791\xa793-\xa795\xa797\xa799\xa79b\xa79d\xa79f\xa7a1\xa7a3\xa7a5\xa7a7\xa7a9\xa7b5\xa7b7\xa7fa\xab30-\xab5a\xab60-\xab65\xab70-\xabbf\xfb00-\xfb06\xfb13-\xfb17\xff41-\xff5a\x10428-\x1044f\x104d8-\x104fb\x10cc0-\x10cf2\x118c0-\x118df\x1d41a-\x1d433\x1d44e-\x1d454\x1d456-\x1d467\x1d482-\x1d49b\x1d4b6-\x1d4b9\x1d4bb\x1d4bd-\x1d4c3\x1d4c5-\x1d4cf\x1d4ea-\x1d503\x1d51e-\x1d537\x1d552-\x1d56b\x1d586-\x1d59f\x1d5ba-\x1d5d3\x1d5ee-\x1d607\x1d622-\x1d63b\x1d656-\x1d66f\x1d68a-\x1d6a5\x1d6c2-\x1d6da\x1d6dc-\x1d6e1\x1d6fc-\x1d714\x1d716-\x1d71b\x1d736-\x1d74e\x1d750-\x1d755\x1d770-\x1d788\x1d78a-\x1d78f\x1d7aa-\x1d7c2\x1d7c4-\x1d7c9\x1d7cb\x1e922-\x1e943_]
$alpha = [$small $large]

$graphic   = [$small $large $symbol $digit $special \:\"\']

$octit     = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \']
$symchar   = [$symbol \:]
$nl        = [\n\r]

@reservedid =
        case|default|else|if|
        infix|infixl|infixr|
        then|family|foreign|export|dynamic|
        safe|threadsafe|unsafe|stdcall|ccall|dotnet

@varid  = $small $idchar* [\#]?
@conid  = $large $idchar* [\#]?
@anyid = (@varid | @conid)
@anyTHid = [$small $large] [$alpha $digit]*
@qual   = (@conid ".")*
@varsym = $symbol $symchar* | [⤜ ⤚ ⤛ ⤙ ★]
@consym = \: $symchar*

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
         | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap

haskell :-

<0> $white+                                     ;

<nestcomm> {
  "{-"                                          { m (subtract 1) (Comment Open) }
  "-}"                                          { m (+1) (Comment Close) }
  $white+                                       ; -- whitespace
  [^\-\{]+                                      { c $ Comment Text } -- rule to generate comments larger than 1 char
  .                                             { c $ Comment Text }
}

<0> {
-- The first rule matches operators that begin with --, eg --++-- is a valid
-- operator and *not* a comment.
-- Note that we have to dissallow '-' as a symbol char for the first one
-- of these because we may have -------- which would stilljust be the
-- start of a comment.
  "--"\-* [$symbol # \-] $symchar*              { cs Operator }
-- The next rule allows for the start of a comment basically
-- it is -- followed by anything which isn't a symbol character
-- (OR more '-'s). So for example "-----:" is still the start of a comment.
  "--"~[$symbol # \-][^$nl]*                    { c $ Comment Line }
-- Finally because the above rule had to add in a non symbol character
-- it's also possible that we have just finishing a line,
-- people sometimes do this for example  when breaking up paragraphs
-- in a long comment.
  "--"$nl                                       { c $ Comment Line }

  "{-"                                          { m (subtract 1) $ Comment Open }

  ^"#".*                                        { c $ CppDirective }
  $special                                      { cs $ \(c:_) -> Special c }
  "deriving"                                    { c (Reserved Deriving) }
  "forall"                                      { c (Reserved Forall) }
  "∀"                                           { c (Reserved Forall) }
  @reservedid                                   { c (Reserved Other) }
  "hiding"                                      { c (Reserved Hiding) }
  "module"                                      { c (Reserved Module) }
  "type"                                        { c (Reserved Type) }
  "newtype"                                     { c (Reserved NewType) }
  "as"                                          { c (Reserved As) }
  "import"                                      { c (Reserved Import) }
  "data"                                        { c (Reserved Data) }
  "where"                                       { c (Reserved Where) }
  "qualified"                                   { c (Reserved Qualified) }
  "let"                                         { c (Reserved Let) }
  "in"                                          { c (Reserved In) }
  "of"                                          { c (Reserved Of) }
  "do" | "mdo"                                  { c (Reserved Do) }
  "class"                                       { c (Reserved Class) }
  "instance"                                    { c (Reserved Instance) }
  `@qual @varid`                                { cs $ Operator . init . tail }
  `@qual @conid`                                { cs $ ConsOperator . init . tail }
  @qual @varid                                  { c VarIdent }
  @qual @conid                                  { c ConsIdent }

  "|"                                           { c (ReservedOp Pipe) }
  "="                                           { c (ReservedOp Equal) }
  \\                                            { c (ReservedOp BackSlash) }
  "<-" | "←"                                    { c (ReservedOp LeftArrow) }
  "->" | "→"                                    { c (ReservedOp RightArrow) }
  ".."                                          { c (ReservedOp DoubleDot) }
  "@"                                           { c (ReservedOp Arobase) }
  "~"                                           { c (ReservedOp Tilda) }
  "=>" | "⇒"                                    { c (ReservedOp DoubleRightArrow) }
  "::" | "∷"                                    { c (ReservedOp DoubleColon) }
  @qual @varsym                                 { cs Operator }
  @qual @consym                                 { cs ConsOperator }

  @decimal
    | 0[oO] @octal
    | 0[xX] @hexadecimal                        { c Number }

  @decimal \. @decimal @exponent?
    | @decimal @exponent                        { c Number }

  \'\' @anyid                                   { c THQuote } -- type
  \' @anyTHid                                   { c THQuote } -- expression
  \' ($graphic # [\'\\] | " " | @escape) \'     { c CharTok }
  \" @string* \"                                { c StringTok }
  .                                             { c Unrecognized }
}

{

type HlState = Int

data CommentType = Open | Close | Text | Line
    deriving (Eq, Show)

data ReservedType = Hiding | Qualified | As | Import | Data | NewType | Type | Where
                  | Let | In | Do | Of | OtherLayout | Deriving | Module | Forall | Other | Class | Instance
    deriving (Eq, Show)

data OpType = Pipe | Equal | BackSlash | LeftArrow | RightArrow | DoubleRightArrow | DoubleColon | DoubleDot | Arobase | Tilda
    deriving (Eq, Show)

data Token = Number | CharTok | StringTok | VarIdent | ConsIdent
           | Reserved !ReservedType | ReservedOp !OpType | Special Char
           | ConsOperator String | Operator String
           | Comment !CommentType
           | THQuote
           | CppDirective | Unrecognized
             deriving (Eq, Show)

tokenToStyle :: Token -> StyleName
tokenToStyle tok = case tok of
  CppDirective       -> preprocessorStyle
  Number             -> numberStyle
  CharTok            -> stringStyle
  StringTok          -> stringStyle
  VarIdent           -> variableStyle
  ConsIdent          -> typeStyle
  ReservedOp _       -> operatorStyle
  Reserved Import    -> importStyle
  Reserved Qualified -> importStyle
  Reserved As        -> importStyle
  Reserved Hiding    -> importStyle
  Reserved _         -> keywordStyle
  Special _          -> defaultStyle
  ConsOperator _     -> operatorStyle
  Operator _         -> operatorStyle
  Comment _          -> commentStyle
  THQuote            -> quoteStyle
  Unrecognized       -> errorStyle

tokenToText :: Token -> Maybe String
tokenToText (ReservedOp BackSlash) = Just "λ"
tokenToText (ReservedOp RightArrow) = Just "→ "
tokenToText (ReservedOp DoubleRightArrow) = Just "⇒ "
tokenToText (ReservedOp LeftArrow) = Just "← "
tokenToText (ReservedOp DoubleColon) = Just "∷ "
-- missing: ++ >>=
tokenToText (Operator "*") = Just "×"
tokenToText (Operator "-") = Just "−"
-- tokenToText (Operator "-->") = Just " ⟶ "
tokenToText (Operator ".") = Just "·"
tokenToText (Operator "/=") = Just "≠ "
-- tokenToText (Operator "<--") = Just " ⟵ "
tokenToText (Operator "<-|") = Just " ↤ "
-- tokenToText (Operator "<<") = Just "⟪ "
tokenToText (Operator "<|") = Just "◃ "
tokenToText (Operator "<~") = Just "↜ "
tokenToText (Operator "==") = Just "≡ "
-- tokenToText (Operator "==>") = Just " ⟹ "
tokenToText (Operator "=?") = Just "≟ "
-- tokenToText (Operator ">>") = Just "⟫ "
tokenToText (Operator "|-->") = Just " ⟼  "
tokenToText (Operator "|->") = Just " ↦ "
tokenToText (Operator "|>") = Just "▹ "
tokenToText (Operator "~=") = Just "≃ "
tokenToText (Operator "~>") = Just "↝ "
tokenToText (Operator ">=") = Just "≥ "
tokenToText (Operator "<=") = Just "≤ "
tokenToText (Operator "-<") = Just "↢ "
tokenToText (Operator "&&") = Just "∧ "
tokenToText (Operator "||") = Just "∨ "
{- these are not operators
tokenToText (Operator "_|_") = Just " ⊥ "
tokenToText (Operator "exists") = Just "    ∃ "
tokenToText (Operator "not") = Just " ¬ "
tokenToText (Operator "neg") = Just " ¬ "
-}
tokenToText (Reserved Forall) = Just "    ∀ "
tokenToText _ = Nothing

startsLayout (Reserved Do) = True
startsLayout (Reserved Of) = True
startsLayout (Reserved Where) = True
startsLayout (Reserved Let) = True
startsLayout (Reserved OtherLayout) = True
startsLayout _ = False

isComment (Comment _) = True
isComment _ = False

stateToInit x | x < 0     = nestcomm
              | otherwise = 0

initState :: HlState
initState = 0

type TT = Tok Token

isSpecial :: String -> Token -> Bool
isSpecial cs (Special c) = c `elem` cs
isSpecial _  _ = False

isErrorTok :: Token -> Bool
isErrorTok = isSpecial "!"


#include "common.hsinc"
}
