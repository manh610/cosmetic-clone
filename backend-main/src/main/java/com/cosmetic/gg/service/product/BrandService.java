package com.cosmetic.gg.service.product;

import java.util.List;

import org.springframework.web.multipart.MultipartFile;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.entity.product.Brand;
import com.cosmetic.gg.model.product.BrandModel;

public interface BrandService {
	
	List<Brand> search(String keyword, EStatus status);
	
	Brand getById(String id);

	List<Error> validator(BrandModel brandModel);
	
	Brand create(BrandModel brandModel);
	
	Brand update(BrandModel brandModel);
	
	Error delete(String id);
	
	ErrorCode changeImage(MultipartFile imageFile, String id);
}
