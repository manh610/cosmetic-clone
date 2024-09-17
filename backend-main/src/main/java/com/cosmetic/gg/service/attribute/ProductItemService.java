package com.cosmetic.gg.service.attribute;

import java.util.List;

import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.entity.attribute.ProductItem;

public interface ProductItemService {

	List<Error> validator(ProductItem productItem);
	
	ProductItem create(ProductItem productItem);
	
	Error delete(String id);
}
