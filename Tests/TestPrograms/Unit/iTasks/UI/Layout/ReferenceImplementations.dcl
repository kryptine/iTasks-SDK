definition module iTasks.UI.Layout.ReferenceImplementations

import iTasks.UI.Layout

applyLayoutRule :: LayoutRule UI -> UI 

//Reference layouts of all core layouts for testing
setUITypeRef_            :: UIType -> (UI -> UI)
setUIAttributesRef_      :: UIAttributes -> (UI -> UI)
delUIAttributesRef_      :: UIAttributeSelection -> (UI -> UI)
modifyUIAttributesRef_   :: UIAttributeSelection (UIAttributes -> UIAttributes) -> (UI -> UI)
copySubUIAttributesRef_  :: UIAttributeSelection UIPath UIPath -> (UI -> UI)
wrapUIRef_               :: UIType -> (UI -> UI)
unwrapUIRef_             :: (UI -> UI)
insertChildUIRef_        :: Int UI -> (UI -> UI)
removeSubUIsRef_         :: UISelection -> (UI -> UI)
moveSubUIsRef_           :: UISelection UIPath Int -> (UI -> UI)
layoutSubUIsRef_         :: UISelection LayoutRule -> (UI -> UI)
sequenceLayoutsRef_      :: LayoutRule LayoutRule -> (UI -> UI)

