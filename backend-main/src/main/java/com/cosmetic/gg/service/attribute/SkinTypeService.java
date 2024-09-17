package com.cosmetic.gg.service.attribute;

import java.util.List;

import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.entity.attribute.SkinType;

public interface SkinTypeService {
	
	List<SkinType> search();

	List<Error> validator(SkinType skinType);
	
	SkinType create(SkinType skinType);
	
	SkinType update(SkinType skinType);
	
	SkinType getById(String id);
	
	Error delete(String id);
}
