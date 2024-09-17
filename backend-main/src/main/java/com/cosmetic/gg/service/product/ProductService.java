package com.cosmetic.gg.service.product;

import java.util.List;
import java.util.Map;

import org.springframework.web.multipart.MultipartFile;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.dto.response.attribute.ValueDetailResponse;
import com.cosmetic.gg.dto.response.product.ProductDetailResponse;
import com.cosmetic.gg.entity.product.Product;
import com.cosmetic.gg.model.product.ProductModel;

public interface ProductService {
	
	Map<String, Object> search(
			String keyword, 
			EStatus status,
			String brandId, 
			String categoryId, 
			String skinTypeId, 
			Float min,
			Float max,
			Boolean isDate,
			Integer pageIndex, 
			Integer pageSize);

	List<Error> validator(ProductModel productModel);
	
	Product create(ProductModel productModel);
	
	Product update(ProductModel productModel);
	
	Error delete(String id);
	
	ErrorCode deleteMultiImage(List<String> ids);
	
	ErrorCode changePhoto(MultipartFile photo, String id);
	
	ErrorCode deleteImage(String id);
	
	ErrorCode changeImage(List<MultipartFile> images, String id);
	
	ProductDetailResponse detail(String id);
	
	ValueDetailResponse getValueByProductItem(String id);
}
