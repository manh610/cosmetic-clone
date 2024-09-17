package com.cosmetic.gg.service.attribute;

import java.util.List;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.entity.attribute.Attribute;

public interface AttributeService {

	List<Attribute> search(EStatus status);
	
	List<Error> validator(Attribute attribute);
	
	Attribute create(Attribute attribute);
	
	Attribute update(Attribute attribute);
	
	Error delete(String id);
	
	Attribute getById(String id);
}
